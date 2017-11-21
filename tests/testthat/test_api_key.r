context("Using API keys")

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
    expect_equal(sleep_time(payload$args), 0.1)
    # No key = 3 per sec
    set_entrez_key("")
    payload <- make_entrez_query(util="test", config=list(), id=100, debug_mode=TRUE)
    expect_equal(sleep_time(payload$args), 1/3)
})

