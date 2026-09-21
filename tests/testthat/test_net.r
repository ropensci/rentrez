context("Network")
test_that("The NCBI is contactable from this comptuter /",{
    response <- net(httr::GET("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"))
    skip_if(response$status_code >= 400, "NCBI returned an error status")
    expect_lt(response$status_code, 400)
})
