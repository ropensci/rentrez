context("POST (the HTTP verb)")

are_there_any_cancer_papers <- entrez_search(db="pubmed", term="Cancer", retmax=201)
search_ids <- are_there_any_cancer_papers$ids

test_that("We can POST to NCBI epost", {
    wh <- entrez_post(db="pubmed", id=search_ids)
    expect_that(wh, is_a("web_history"))
    expect_that(as.integer(wh$QueryKey), is_a("integer"))
    expect_false(is.na(as.integer(wh$QueryKey)))    
})

test_that("We can fecth using POST", {
    fetched_ids <- entrez_fetch(db="pubmed", id=search_ids, rettype="uilist")
    expect( 
        all( strsplit(fetched_ids, "\n")[[1]] %in% search_ids), 
        "fetched IDs do not match sent IDs when using httr::POST"
    )
})
