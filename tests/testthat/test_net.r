context("Network")
test_that("The NCBI is contactable from this comptuter /",{
    expect_true(!httr::http_error("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"))
})
