context("error handling")

# An <ERROR> response from NCBI (e.g. a bad database name) used to crash the
# esearch parser with a cryptic "subscript out of bounds". It should instead
# surface the NCBI message and fail clearly. No network needed.
err_doc <- function()
    XML::xmlTreeParse(
        "<eSearchResult><ERROR>Invalid db name specified: nonsense</ERROR></eSearchResult>",
        useInternalNodes = TRUE)

test_that("parse_esearch warns with the NCBI message on an error response", {
    expect_warning(try(rentrez:::parse_esearch(err_doc(), history = FALSE), silent = TRUE),
                   "Invalid db name")
})

test_that("parse_esearch fails clearly (not 'subscript out of bounds')", {
    expect_error(suppressWarnings(rentrez:::parse_esearch(err_doc(), history = FALSE)),
                 "no result")
})

test_that("entrez_check reports the query URL on an HTTP failure", {
    fake_response <- list(status_code = 414L, url = "https://eutils.ncbi.nlm.nih.gov/bad")
    expect_error(rentrez:::entrez_check(fake_response),
                 "https://eutils.ncbi.nlm.nih.gov/bad")
})

# A count-only response (rettype="count") has a Count but no RetMax/IdList,
# which used to crash the parser. No network needed.
test_that("parse_esearch handles a count-only response", {
    doc <- XML::xmlTreeParse("<eSearchResult><Count>5648701</Count></eSearchResult>",
                             useInternalNodes = TRUE)
    res <- rentrez:::parse_esearch(doc, history = FALSE)
    expect_equal(res$count, 5648701L)
    expect_true(is.na(res$retmax))
})
