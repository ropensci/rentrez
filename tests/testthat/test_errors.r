context("error handling")

# An <ERROR> response from NCBI (e.g. a bad database name) used to crash the
# esearch parser with a cryptic "subscript out of bounds". It should instead
# surface the NCBI message and fail clearly. No network needed.
err_doc <- function()
    XML::xmlTreeParse(
        "<eSearchResult><ERROR>Invalid db name specified: nonsense</ERROR></eSearchResult>",
        useInternalNodes = TRUE)

# NCBI reports a bad request in the body of an ordinary HTTP 200 reply, as an
# <ERROR> node in xml and an ERROR field in json.
err_json <- function()
    list(esearchresult = list(ERROR = "Invalid db name specified: nonsense"))

test_that("parse_esearch warns with the NCBI message on an error response", {
    expect_warning(expect_error(rentrez:::parse_esearch(err_doc(), history = FALSE)),
                   "Invalid db name")
})

# The reason has to reach the error itself. A caller looping over searches sees
# only conditionMessage(), and an error pointing at a warning tells them nothing.
test_that("parse_esearch carries the NCBI message in the error, both formats", {
    for (doc in list(err_doc(), err_json())) {
        msg <- tryCatch(suppressWarnings(rentrez:::parse_esearch(doc, history = FALSE)),
                        error = conditionMessage)
        expect_true(grepl("no result", msg, fixed = TRUE))
        expect_true(grepl("Invalid db name specified: nonsense", msg, fixed = TRUE))
        expect_false(grepl("subscript out of bounds", msg, fixed = TRUE))
    }
})

# A search that simply found nothing is a result, not a failure, and json keeps
# its count in that case. Guarding on the ERROR field alone would be enough, but
# a reply missing count entirely cannot be parsed either.
test_that("a search with no hits is not treated as a failure", {
    none <- list(esearchresult = list(count = "0", retmax = "0", idlist = list(),
                                      querytranslation = "nothing[All Fields]"))
    res <- rentrez:::parse_esearch(none, history = FALSE)
    expect_equal(res$count, 0L)
    expect_true(inherits(res, "esearch"))
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
