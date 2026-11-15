context("POST (the HTTP verb)")
skip_on_cran()

#setup (guarded so transient NCBI problems skip rather than abort the file)
ncbi_ok <- tryCatch({
    are_there_any_cancer_papers <- entrez_search(db="pubmed", term="Cancer", retmax=201)
    search_ids <- are_there_any_cancer_papers$ids
    TRUE
}, error = ncbi_setup_failed)

test_that("We can POST to NCBI epost", {
    skip_without_ncbi(ncbi_ok)
    wh <- net(entrez_post(db="pubmed", id=search_ids))
    expect_that(wh, is_a("web_history"))
    expect_that(as.integer(wh$QueryKey), is_a("integer"))
    expect_false(is.na(as.integer(wh$QueryKey)))
})

test_that("We can fecth using POST", {
    skip_without_ncbi(ncbi_ok)
    fetched_ids <- net(entrez_fetch(db="pubmed", id=search_ids, rettype="uilist"))
    expect(
        all( strsplit(fetched_ids, "\n")[[1]] %in% search_ids),
        "fetched IDs do not match sent IDs when using httr::POST"
    )
})

test_that("A search term too long for a url goes through", {
    #301 publication years make a url that NCBI refuses with HTTP 414 when it
    #arrives by GET (#163)
    every_year <- paste0(1800:2100, "[dp]", collapse=" OR ")
    long_search <- net(entrez_search(db="pubmed", term=every_year, retmax=0))
    expect_that(long_search, is_a("esearch"))
    expect_true(long_search$count > 0)
})
