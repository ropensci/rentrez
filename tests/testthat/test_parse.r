context("result-parsers")


raw_rec <- entrez_fetch(db="pubmed", id=20674752, rettype="xml")
xml_rec <- entrez_fetch(db="pubmed", id=20674752, rettype="xml", parsed=TRUE)
multi_rec <- entrez_fetch(db="pubmed", 
                           id=c(22883857, 25042335, 20203609,11959827),
                           rettype="xml", parsed=TRUE)
parsed_raw <- parse_pubmed_xml(raw_rec)
parsed_rec <- parse_pubmed_xml(xml_rec)
parsed_multi <- parse_pubmed_xml(multi_rec)

test_that("pubmed file parsers work",{
    expect_that(raw_rec, is_a("character"))

    expect_that(parsed_raw, is_a("pubmed_record"))
    expect_that(parsed_rec, is_a("pubmed_record"))
    expect_that(names(parsed_rec), is_a("character"))
    expect_that(parsed_rec$pmid, is_identical_to("20674752"))

    expect_that(parsed_multi, is_a("multi_pubmed_record"))
    expect_that(parsed_multi[[1]], is_a("pubmed_record"))
    expect_that(length(parsed_multi), equals(4))

    # Older (buggier) versions of the pubmed parser included data from every
    # record in an xml file in each parsed record. If that error is
    # re-introduced there will be 25 authors in each record and this will fail
    expect_that(length(parsed_multi[[1]]$authors), equals(1))

})

test_that("we can print pubmed records", {
    expect_output(parsed_rec, "Pubmed record")
    expect_output(parsed_multi, "List of 4 pubmed records")
})

test_that("We warn about unknown pubmed record types", {
    rec = entrez_fetch(db="pubmed", id=25905152, rettype="xml")
    expect_warning(parsed_rec <- parse_pubmed_xml(rec))
    expect_output(parsed_rec, "Pubmed record \\(empty\\)")
})

   
   
