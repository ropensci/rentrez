context("POST (the HTTP verb)")

#setup (guarded so transient NCBI problems skip rather than abort the file)
ncbi_ok <- tryCatch({
    are_there_any_cancer_papers <- entrez_search(db="pubmed", term="Cancer", retmax=201)
    search_ids <- are_there_any_cancer_papers$ids
    TRUE
}, error = function(e) {
    message("NCBI not available: ", conditionMessage(e))
    FALSE
})

test_that("We can POST to NCBI epost", {
    skip_if(!ncbi_ok, "NCBI not available")
    wh <- net(entrez_post(db="pubmed", id=search_ids))
    expect_that(wh, is_a("web_history"))
    expect_that(as.integer(wh$QueryKey), is_a("integer"))
    expect_false(is.na(as.integer(wh$QueryKey)))
})

test_that("We can fecth using POST", {
    skip_if(!ncbi_ok, "NCBI not available")
    fetched_ids <- net(entrez_fetch(db="pubmed", id=search_ids, rettype="uilist"))
    expect(
        all( strsplit(fetched_ids, "\n")[[1]] %in% search_ids),
        "fetched IDs do not match sent IDs when using httr::POST"
    )
})
