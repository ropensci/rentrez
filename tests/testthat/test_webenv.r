context("WebEnv")
test_that("Searches using WebEnv features work", {
    #setup
    web_env_search <- entrez_search(db="nuccore", 
                                    term="Gastropoda[Organism] AND COI[Gene]", 
                                    use_history=TRUE)
    wh <- web_env_search$web_history
    snail_coi <- entrez_fetch(db = "nuccore", web_history=wh,  rettype = "fasta", retmax = 10)
    
    #test
    expect_that(wh$WebEnv, is_a("character"))
    expect_that(as.integer(wh$QueryKey), is_a("integer"))
    expect_that(snail_coi, is_a("character"))
    expect_that(length(strsplit(snail_coi, ">")[[1]]), equals(11))
})
