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

# The query URL carries the caller's API key, and these errors get pasted into
# bug reports, so the key has to come out before the message is built.
test_that("entrez_check keeps the API key out of the error message", {
    keyed <- function(code) list(
        status_code = code,
        url = paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
                     "?db=pubmed&api_key=SECRET_abc123&tool=rentrez"))
    for (code in c(414L, 502L)) {
        msg <- tryCatch(rentrez:::entrez_check(keyed(code)), error = conditionMessage)
        expect_false(grepl("SECRET_abc123", msg, fixed = TRUE))
        expect_true(grepl("api_key=<redacted>", msg, fixed = TRUE))
    }
})

test_that("redact_key leaves a URL without a key alone", {
    plain <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed"
    expect_equal(rentrez:::redact_key(plain), plain)
})
