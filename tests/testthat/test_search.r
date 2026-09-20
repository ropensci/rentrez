context("search")

#setup (guarded so transient NCBI problems skip rather than abort the file)
ncbi_ok <- tryCatch({
    gsearch <- entrez_global_query("Heliconius")
    pubmed_search <- entrez_search(db = "pubmed",
                                   term = "10.1016/j.ympev.2010.07.013[doi]")
    json_search <- entrez_search(db="pubmed",
                                 term =  "10.1016/j.ympev.2010.07.013[doi]",
                                 retmode='json')
    TRUE
}, error = ncbi_setup_failed)

test_that("Global query works",{
    skip_without_ncbi(ncbi_ok)
    #global query
    expect_that(gsearch, is_a("numeric"))
    expect_that(names(gsearch), is_a("character"))
    #now includes 'database error' for some databaes
    #these are made NA in entrez_global_query, which seems reasonable
    expect_true(sum(gsearch, na.rm=TRUE) > 0 )
})

test_that("Entrez query works",{
    skip_without_ncbi(ncbi_ok)
    #entrez query
    expect_that(pubmed_search, is_a("esearch"))
    expect_that(pubmed_search$ids, is_identical_to("20674752"))
})

test_that("Entrez query works just as well with xml/json",{
    skip_without_ncbi(ncbi_ok)
    expect_that(json_search, is_a("esearch"))
    expect_that(json_search$ids, is_identical_to("20674752"))
    expect_equal(names(pubmed_search),names(json_search))
})


test_that("we can print search results", {
    skip_without_ncbi(ncbi_ok)
    expect_output(print(pubmed_search), "Entrez search result with \\d+ hits")
    expect_output(print(json_search),   "Entrez search result with \\d+ hits")
})
