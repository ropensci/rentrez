context("elink error handling")

# NCBI answers a command it will not serve with HTTP 200 and an <ERROR> inside
# the LinkSet, so the response looks fine until a parser reads it. The parsers
# used to run on and die on a subscript or a names mismatch, which told the
# reader nothing about the cause. No network needed for any of this.
err_linkset <- function() {
    doc <- XML::xmlTreeParse(
        paste0("<eLinkResult><LinkSet><ERROR>Exception:\n",
               "llinkslib command not supported</ERROR></LinkSet></eLinkResult>"),
        useInternalNodes = TRUE)
    doc["//LinkSet"][[1]]
}

test_that("linkout commands report the NCBI message", {
    for (cmd in c("llinks", "llinkslib", "prlinks")) {
        f <- rentrez:::make_elink_fxn(cmd)
        expect_error(f(err_linkset()), "no linkouts", info = cmd)
        expect_error(f(err_linkset()), "llinkslib command not supported", info = cmd)
    }
})

test_that("check commands report the NCBI message", {
    for (cmd in c("ncheck", "lcheck")) {
        f <- rentrez:::make_elink_fxn(cmd)
        expect_error(f(err_linkset()), "no IdCheckList", info = cmd)
        expect_error(f(err_linkset()), "llinkslib command not supported", info = cmd)
    }
})

# An id can come back without the attribute the check command asked for, which
# leaves the ids and the flags uneven. Naming one vector with the other then
# raises the same opaque error the guard above exists to remove.
test_that("check commands report an uneven id and flag count", {
    linkset <- function(inner) {
        doc <- XML::xmlTreeParse(paste0("<eLinkResult><LinkSet>", inner,
                                        "</LinkSet></eLinkResult>"),
                                 useInternalNodes = TRUE)
        doc["//LinkSet"][[1]]
    }
    no_attr <- linkset("<IdCheckList><Id>123</Id></IdCheckList>")
    partial <- linkset(paste0("<IdCheckList><Id HasNeighbor=\"Y\">1</Id>",
                              "<Id>2</Id></IdCheckList>"))
    f <- rentrez:::make_elink_fxn("ncheck")
    expect_error(f(no_attr), "1 ids but 0 HasNeighbor flags")
    expect_error(f(partial), "2 ids but 1 HasNeighbor flags")

    ok <- linkset(paste0("<IdCheckList><Id HasNeighbor=\"Y\">1</Id>",
                         "<Id HasNeighbor=\"N\">2</Id></IdCheckList>"))
    expect_equal(length(f(ok)$check), 2)
})

# An id with nothing to link is a result, not a failure. Only a reply where NCBI
# said something went wrong should stop.
test_that("an id with no linkouts returns an empty set rather than an error", {
    linkset <- function(inner) {
        doc <- XML::xmlTreeParse(paste0("<eLinkResult><LinkSet>", inner,
                                        "</LinkSet></eLinkResult>"),
                                 useInternalNodes = TRUE)
        doc["//LinkSet"][[1]]
    }
    f <- rentrez:::make_elink_fxn("llinks")

    quiet <- f(linkset("<IdUrlList></IdUrlList>"))
    expect_true(inherits(quiet, "elink"))
    expect_equal(length(quiet$linkouts), 0)

    # the same shape, but NCBI explained itself, so this one stops
    loud <- linkset("<IdUrlList></IdUrlList><ERROR>db is closed today</ERROR>")
    expect_error(f(loud), "db is closed today")
})

test_that("parse_elink fails clearly when the reply holds no LinkSet", {
    doc <- XML::xmlTreeParse("<eLinkResult></eLinkResult>", useInternalNodes = TRUE)
    expect_error(rentrez:::parse_elink(doc, cmd = "llinks", by_id = FALSE, id = 1),
                 "no LinkSet")
})

test_that("the old cryptic errors are gone", {
    msg <- tryCatch(rentrez:::make_elink_fxn("llinks")(err_linkset()),
                    error = conditionMessage)
    expect_false(grepl("subscript out of bounds", msg, fixed = TRUE))
    expect_false(grepl("'names' attribute", msg, fixed = TRUE))
})
