context("WebEnv")
test_that("Searches using WebEnv features work", {
    #setup
    web_env_search <- entrez_search(db="nuccore", 
                                    term="Gastropoda[Organism] AND COI[Gene]", 
                                    usehistory="y")
    cookie <- web_env_search$WebEnv
    qk <- web_env_search$QueryKey 
    snail_coi <- entrez_fetch(db = "nuccore", WebEnv = cookie, query_key = qk,
                              rettype = "fasta", retmax = 10)
    
    #test
    expect_that(cookie, is_a("character"))
    expect_that(qk, is_a("integer"))
    expect_that(snail_coi, is_a("character"))
    expect_that(length(strsplit(snail_coi, ">")[[1]]), equals(11))
})
