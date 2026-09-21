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

# With by_id, NCBI answers each id with its own LinkSet, so the reply is checked
# by count. Matching ids does not work: an accession comes back as a GI number,
# and the check and linkout commands carry no IdList (#238). The LinkSets below
# are trimmed from real replies.
elink_reply <- function(linksets) {
    XML::xmlTreeParse(paste0("<eLinkResult>", paste0(linksets, collapse = ""),
                             "</eLinkResult>"), useInternalNodes = TRUE)
}
neighbor_linkset <- function(gi) paste0(
    "<LinkSet><DbFrom>nuccore</DbFrom><IdList><Id>", gi, "</Id></IdList>",
    "<LinkSetDb><DbTo>protein</DbTo><LinkName>nuccore_protein</LinkName>",
    "<Link><Id>120407068</Id></Link></LinkSetDb></LinkSet>")
ncheck_linkset <- function(id) paste0(
    "<LinkSet><DbFrom>pubmed</DbFrom><IdCheckList>",
    "<Id HasNeighbor=\"Y\">", id, "</Id></IdCheckList></LinkSet>")
llinks_linkset <- function(id) paste0(
    "<LinkSet><DbFrom>pubmed</DbFrom><IdUrlList><IdUrlSet><Id>", id, "</Id>",
    "<ObjUrl><Url>https://doi.org/10.1038/nature08789</Url>",
    "<Category>Full Text Sources</Category><Provider><Name>Nature Publishing",
    " Group</Name><NameAbbr>NPG</NameAbbr><Id>3094</Id></Provider></ObjUrl>",
    "</IdUrlSet></IdUrlList></LinkSet>")

test_that("a full by_id reply raises no warning, whatever its shape", {
    pmids <- c("20203609", "20203610")
    cases <- list(
        list(cmd = "neighbor", sent = c("NM_000546.6", "NM_001126112.3"),
             reply = elink_reply(neighbor_linkset(c("1808862652", "1894803099")))),
        list(cmd = "ncheck", sent = pmids, reply = elink_reply(ncheck_linkset(pmids))),
        list(cmd = "llinks", sent = pmids, reply = elink_reply(llinks_linkset(pmids))))
    for (case in cases) {
        res <- rentrez:::parse_elink(case$reply, cmd = case$cmd, by_id = TRUE)
        expect_no_warning(rentrez:::warn_unanswered_ids(res, case$sent))
    }
})

test_that("a by_id reply short of LinkSets warns with counts, not ids", {
    res <- rentrez:::parse_elink(elink_reply(ncheck_linkset("20203609")),
                                 cmd = "ncheck", by_id = TRUE)
    msg <- tryCatch(rentrez:::warn_unanswered_ids(res, c("20203609", "20203610")),
                    warning = conditionMessage)
    expect_equal(msg, "NCBI returned results for 1 of the 2 IDs requested")
})
