context("einfo functions")
test_that("einfo, and functions built on it, work", {
    #Can we get xml recs from einfo
    einfo_rec <- entrez_info()
    pm_rec <- entrez_info(db="pubmed")
    expect_that(einfo_rec, is_a("XMLInternalDocument"))
    expect_that(pm_rec, is_a("XMLInternalDocument"))
    
    #Can we get summary information on DBs
    dbs <- entrez_dbs()
    expect_that(dbs, is_a("character"))
    expect_true("pubmed" %in% dbs)

    cdd <- entrez_db_summary("cdd")
    expect_that(cdd, is_a("character"))
    expect_named(cdd)

    search_fields <- entrez_db_searchable("pmc")
    sf_df <- as.data.frame(search_fields)
    expect_that(search_fields, is_a("eInfoSearch"))
    expect_named(search_fields$GRNT)
    expect_that(sf_df, is_a("data.frame"))

    omim_links <- entrez_db_links("omim")
    expect_that(omim_links, is_a("eInfoLink"))
    expect_named(omim_links$snp)
    
    omim_df <- as.data.frame(omim_links)
    expect_that(omim_df, is_a("data.frame"))
    expect_equal(nrow(omim_df), length(omim_links))
})
