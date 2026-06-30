context("einfo functions")

#setup (guarded so transient NCBI problems skip rather than abort the file)
ncbi_ok <- tryCatch({
    einfo_rec <- entrez_info()
    pm_rec <- entrez_info(db="pubmed")
    dbs <- entrez_dbs()
    cdd <- entrez_db_summary("cdd")
    search_fields <- entrez_db_searchable("pmc")
    sf_df <- as.data.frame(search_fields)
    omim_links <- entrez_db_links("omim")
    omim_df <- as.data.frame(omim_links)
    TRUE
}, error = function(e) {
    message("NCBI not available: ", conditionMessage(e))
    FALSE
})

test_that(" can get xml recs from einfo", {
    skip_if(!ncbi_ok, "NCBI not available")
    expect_that(einfo_rec, is_a("XMLInternalDocument"))
    expect_that(pm_rec, is_a("XMLInternalDocument"))
})

test_that(" We can get summary information on DBs", {
    skip_if(!ncbi_ok, "NCBI not available")
    expect_that(dbs, is_a("character"))
    expect_true("pubmed" %in% dbs)

    expect_that(cdd, is_a("character"))
    expect_named(cdd)
})

test_that("We can retrieve serach fields", {
    skip_if(!ncbi_ok, "NCBI not available")
    expect_that(search_fields, is_a("eInfoSearch"))
    expect_named(search_fields$GRNT)
    expect_that(sf_df, is_a("data.frame"))
})

test_that("We can retreive linked dbs", {
    skip_if(!ncbi_ok, "NCBI not available")
    expect_that(omim_links, is_a("eInfoLink"))
    expect_named(omim_links[[1]])
    expect_that(omim_df, is_a("data.frame"))
    expect_equal(nrow(omim_df), length(omim_links))
})

test_that("We can prink elink objects", {
    skip_if(!ncbi_ok, "NCBI not available")
    expect_output(print(omim_links), "Databases with linked records for database 'omim'")
    expect_output(print(search_fields), "Searchable fields for database 'pmc'")
})

test_that("We can print elements from einfo object", {
    skip_if(!ncbi_ok, "NCBI not available")
    expect_output(print(omim_links$gene), "Name: omim_gene\n")
    expect_output(print(search_fields$GRNT), "Name: GRNT\n")
    expect_output(print(cdd), "DbName: cdd")
})
