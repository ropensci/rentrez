context("fetching records")


pop_ids = c("307082412", "307075396", "307075338", "307075274")
coi <- entrez_fetch(db = "popset", id = pop_ids[1], 
                    rettype = "fasta")

test_that("Fetching sequences works", {
     expect_that(length(strsplit(coi, ">")[[1]]), equals(30))
          
})

