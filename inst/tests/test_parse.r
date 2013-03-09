test_that("file parsers work",{
    rec <- entrez_fetch(db="pubmed", ids=20674752, file_format="xml")
    parsed_rec <- parse_pubmed_xml(rec)

    expect_that(parsed_rec, is_a("pubmed_record"))
    expect_that(names(parsed_rec), is_a("character"))
    expect_that(parsed_rec$pmid, is_identical_to("20674752"))
})




   
    
