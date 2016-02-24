context("einfo functions")

einfo_rec <- entrez_info()
pm_rec <- entrez_info(db="pubmed")

test_that(" can get xml recs from einfo", {
    expect_that(einfo_rec, is_a("XMLInternalDocument"))
    expect_that(pm_rec, is_a("XMLInternalDocument"))
})

dbs <- entrez_dbs()
cdd <- entrez_db_summary("cdd")

test_that(" We can get summary information on DBs", {
    expect_that(dbs, is_a("character"))
    expect_true("pubmed" %in% dbs)

    expect_that(cdd, is_a("character"))
    expect_named(cdd)
})

search_fields <- entrez_db_searchable("pmc")
sf_df <- as.data.frame(search_fields)

test_that("We can retrieve serach fields", {
    expect_that(search_fields, is_a("eInfoSearch"))
    expect_named(search_fields$GRNT)
    expect_that(sf_df, is_a("data.frame"))
})

omim_links <- entrez_db_links("omim")
omim_df <- as.data.frame(omim_links)

test_that("We can retreive linked dbs", {
    expect_that(omim_links, is_a("eInfoLink"))
    expect_named(omim_links[[1]])    
    expect_that(omim_df, is_a("data.frame"))
    expect_equal(nrow(omim_df), length(omim_links))
})

test_that("We can prink elink objects", {
    expect_output(omim_links, "Databases with linked records for database 'omim'")
    expect_output(search_fields, "Searchable fields for database 'pmc'")
})

test_that("We can print elements from einfo object", {
    expect_output(omim_links$gene, "Name: omim_gene\n")
    expect_output(search_fields$GRNT, "Name: GRNT\n")
    expect_output(cdd, "DbName: cdd")
})
