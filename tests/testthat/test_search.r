context("search")
test_that("search functions work",{
    #setup
    gsearch <- entrez_global_query("Heliconius")
    pubmed_search <- entrez_search(db = "pubmed", 
                                   term = "10.1016/j.ympev.2010.07.013[doi]")

    #tests
    #global query
    expect_that(gsearch, is_a("numeric"))
    expect_that(names(gsearch), is_a("character"))
    expect_true(sum(gsearch) > 0 )
    #entrez query
    expect_that(pubmed_search, is_a("esearch"))
    expect_that(pubmed_search$ids, is_identical_to("20674752"))

})

