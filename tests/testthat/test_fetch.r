context("fetching records")


pop_ids = c("307082412", "307075396", "307075338", "307075274")
coi <- entrez_fetch(db = "popset", id = pop_ids[1], 
                    rettype = "fasta")
xml_rec <- entrez_fetch(db = "popset", id=pop_ids[1], rettype="xml", parsed=TRUE)


test_that("httr does no warn about inferred encoding", {
    expect_message(
       raw_rec <- entrez_fetch(db = "popset", id=pop_ids[1], rettype="xml"), NA
    )
})



test_that("Fetching sequences works", {
     expect_that(length(strsplit(coi, ">")[[1]]), equals(30))
          
})

test_that("Entrez_fetch record parsing works", {
     expect_that(raw_rec, is_a("character"))
     expect_that(xml_rec, is_a("XMLInternalDocument"))
     expect_error( 
       entrez_fetch(db="popset", id="307082412", rettype="fasta", parsed=TRUE), 
       "At present, entrez_fetch can only parse XML records, got fasta"
     )
})



