context("result-parsers")
test_that("file parsers work",{
    one_rec <- entrez_fetch(db="pubmed", id=20674752, rettype="xml")
    three_recs <- entrez_fetch(db="pubmed", 
                              id=c(20203609,11959827,19409887),
                              rettype="xml")
    parsed_rec <- parse_pubmed_xml(one_rec)

    expect_that(parsed_rec, is_a("pubmed_record"))
    expect_that(names(parsed_rec), is_a("character"))
    expect_that(parsed_rec$pmid, is_identical_to("20674752"))

    parsed_multi <- parse_pubmed_xml(three_recs)
    expect_that(parsed_multi, is_a("multi_pubmed_record"))
    expect_that(parsed_multi[[1]], is_a("pubmed_record"))
    expect_that(length(parsed_multi), equals(3))

    # Older (buggier) versions of the pubmed parser included data from every
    # record in an xml file in each parsed record. If that error is
    # re-introduced there will be 25 authors in each record and this will fail
    expect_that(length(parsed_multi[[1]]$authors), equals(6))

})




   
   
