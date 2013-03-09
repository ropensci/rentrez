test_that("search functions work"\{
    #setup
    gsearch <- entrez_global_query("Heliconius")
    pubmed_search <- entrez_search(db = "pubmed", 
                                   term = "10.1016/j.ympev.2010.07.013[doi]")
    web_env_search <- entrez_search(db="nuccore", 
                                    term="Gastropoda[Organism] AND COI[Gene]", 
                                    usehistory="y")
    cookie <- web_env_search$WebEnv
    qk <- web_env_search$QueryKey
    #tests
    #global query
    expect_that(gsearch, is_a("numeric"))
    expect_that(names(gsearch), is("character"))
    expect_true(sum(gsearch) > 0 )
    #entrez query
    expect_that(pubmed_search, is_a("esearch"))
    expect_that(pubmed_search$ids, equals(20674752))
    #entrez with WebEnv
    expect_that(cookie, is_a("character"))
    expect_that(qk, is_a("integer"))
})

