context("WebEnv")
skip_on_cran()
test_that("Searches using WebEnv features work", {
    #setup
    web_env_search <- net(entrez_search(db="nuccore",
                                    term="Gastropoda[Organism] AND COI[Gene]",
                                    use_history=TRUE))
    wh <- web_env_search$web_history
    snail_coi <- net(entrez_fetch(db = "nuccore", web_history=wh,  rettype = "fasta", retmax = 10))
    
    #test
    expect_that(wh$WebEnv, is_a("character"))
    expect_that(as.integer(wh$QueryKey), is_a("integer"))
    expect_that(snail_coi, is_a("character"))
    expect_that(length(strsplit(snail_coi, ">")[[1]]), equals(11))
})

test_that("A json search returns a web history that works", {
    #the json parser built this history from the wrong field, so it came back
    #empty and the fetch below failed with HTTP 400 (#223)
    json_search <- net(entrez_search(db="nuccore",
                                     term="Gastropoda[Organism] AND COI[Gene]",
                                     retmode="json", use_history=TRUE))
    wh <- json_search$web_history
    snail_coi <- net(entrez_fetch(db = "nuccore", web_history=wh, rettype = "fasta", retmax = 2))

    expect_that(wh$WebEnv, is_a("character"))
    expect_that(wh$QueryKey, is_a("character"))
    expect_that(length(strsplit(snail_coi, ">")[[1]]), equals(3))
})
