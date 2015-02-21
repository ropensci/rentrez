context("elink")
elinks <- entrez_link(dbfrom = "pubmed", id = 20674752, db = "all")

test_that("The record-linking funcitons work",{
    expect_that(elinks, is_a("elink"))
    expect_that(names(elinks), is_a("character"))
    expect_that(elinks$file, is_a("XMLInternalDocument"))
    expect_true(length(elinks$pubmed_nuccore) > 0)
})

test_that("Print elink behaves", {
    expect_output(elinks, "elink result with ids from \\d+")
})
