context("fetching records")


pop_ids = c("307082412", "307075396", "307075338", "307075274")
coi <- entrez_fetch(db = "popset", id = pop_ids[1], 
                    rettype = "fasta")
xml_rec <- entrez_fetch(db = "popset", id=pop_ids[1], rettype="native", parsed=TRUE)
raw_rec <- entrez_fetch(db = "popset", id=pop_ids[1], rettype="native")

acc_old = "AF123456.1"
acc_new = "AF123456.2"

test_that("httr does no warn about inferred encoding", {
    expect_message( entrez_fetch(db = "popset", id=pop_ids[1], rettype="uilist"), NA)
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


test_that("Entrez fetch can download versioned sequences", {
    #The two versions of this sequence have different annotations. We can check
    #that we are getting the correct version of the record by checking the name
    #of each sequence reflects the change in annotation.
    old_rec = entrez_fetch(db="nuccore", id="AF123456.1", rettype="fasta")
    new_rec = entrez_fetch(db="nuccore", id="AF123456.2", rettype="fasta")
    expect_match(old_rec, "testis-specific mRNA")
    expect_match(new_rec, "doublesex and mab-3 related transcription factor")
})

                           
