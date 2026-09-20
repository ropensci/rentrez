context("Using API keys")

# These tests overwrite ENTREZ_KEY, and the last of them leaves it empty. This
# file sorts first, so anything left behind here reaches every other test file:
# an empty key sends the rest of the suite to NCBI unkeyed, at three requests a
# second rather than ten (#216). teardown() runs after the whole file whatever
# is added below, which a restore written at the bottom would not.
original_key <- Sys.getenv("ENTREZ_KEY")
teardown(set_entrez_key(original_key))

test_that("API keys can be passed as normal args", {
    payload <- make_entrez_query(util="test", config=list(), id=100, api_key="ABC", debug_mode=TRUE)
    expect_match(payload$args$api_key, "ABC")
})

test_that("API keys can be passed from ENV vars", {
    set_entrez_key("ABC")
    payload <- make_entrez_query(util="test", config=list(), id=100, debug_mode=TRUE)
    expect_match(payload$args$api_key, "ABC")
})

test_that("Rate limiting changes when API key set", {
    # with key = 10 per sec 
    set_entrez_key("ABC")
    payload <- make_entrez_query(util="test", config=list(), id=100, debug_mode=TRUE)
    expect_equal(sleep_time(payload$args), 0.13)
    # No key = 3 per sec
    set_entrez_key("")
    payload <- make_entrez_query(util="test", config=list(), id=100, debug_mode=TRUE)
    expect_equal(sleep_time(payload$args), 0.35)
})

