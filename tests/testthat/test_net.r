context("Network")
test_that("The NCBI is contactable from this comptuter /",{
    response <- httr::GET("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi")
    expect_lt(response$status_code, 400)
})
