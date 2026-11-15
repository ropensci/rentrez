context("live API test helpers")

# is_network_error decides whether a failing live test is NCBI's fault or
# rentrez's. Getting it wrong in one direction turns a bad day at NCBI into a
# red build; in the other it hides a real regression behind a green skip. The
# second is the one worth testing for, because nobody goes looking for it.
#
# No network needed: the conditions are built here.

http_err <- function(msg) simpleError(msg)

curl_err <- function(msg) {
    structure(class = c("curl_error_couldnt_resolve_host", "curl_error",
                        "error", "condition"),
              list(message = msg, call = NULL))
}

test_that("rate limiting and server faults count as NCBI's", {
    expect_true(is_network_error(http_err("HTTP failure: 429, too many requests. x")))
    expect_true(is_network_error(http_err("HTTP failure: 502, bad gateway. x")))
    expect_true(is_network_error(http_err("HTTP failure: 503\nservice unavailable")))
})

test_that("a transport failure counts, with or without its curl class", {
    expect_true(is_network_error(curl_err("Could not resolve host: eutils.ncbi.nlm.nih.gov")))
    expect_true(is_network_error(http_err("Could not resolve host: eutils.ncbi.nlm.nih.gov")))
    expect_true(is_network_error(http_err("Timeout was reached")))
})

# The failures this package actually produces. Every one of these must be
# reported rather than skipped.
test_that("a rentrez bug is never counted as a network problem", {
    expect_false(is_network_error(http_err("subscript out of bounds")))
    expect_false(is_network_error(http_err("argument is of length zero")))
    expect_false(is_network_error(http_err("'names' attribute [5] must be the same length as the vector [0]")))
    expect_false(is_network_error(http_err("ESearch returned no result. NCBI message: Invalid db name")))
})

test_that("a request rentrez built wrongly is not transient", {
    expect_false(is_network_error(http_err("HTTP failure 414, the request is too large. x")))
    expect_false(is_network_error(http_err("HTTP failure: 400\n{\"error\":\"bad request\"}")))
})

# entrez_check() pastes NCBI's reply body into the same message, so an
# unanchored match would let NCBI's prose decide whether a bug gets reported.
test_that("NCBI's reply body cannot make a failure look transient", {
    expect_false(is_network_error(
        http_err("HTTP failure: 400\n{\"msg\":\"upstream said HTTP failure: 503\"}")))
    expect_false(is_network_error(
        http_err("HTTP failure: 400\n{\"msg\":\"Connection reset by peer\"}")))
})
