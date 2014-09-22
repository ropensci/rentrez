context("Network")
test_that("The NCBI is contactable from this comptuter /",{
    expect_true(httr::url_success("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"))
})
