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
        expect_error(f(err_linkset()), "no linkouts")
        expect_error(f(err_linkset()), "llinkslib command not supported")
    }
})

test_that("check commands report the NCBI message", {
    for (cmd in c("ncheck", "lcheck")) {
        f <- rentrez:::make_elink_fxn(cmd)
        expect_error(f(err_linkset()), "no IdCheckList")
        expect_error(f(err_linkset()), "llinkslib command not supported")
    }
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
