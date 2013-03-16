test_that("Functions to fetch records & summaries work", {
          #setup
          pop_ids = c("307082412", "307075396", "307075338", "307075274")
          pop_summ <- entrez_summary(db="popset", id=pop_ids)
          coi <- entrez_fetch(db = "popset", id = pop_ids[1], 
                              rettype = "fasta")

          #tests
          expect_that(pop_summ[[1]], is_a("esummary"))
          expect_that(length(pop_summ), equals(4))
          sapply(pop_summ, function(x)
                 expect_that(x[["Title"]], matches("Muraenidae")))
          expect_that(length(strsplit(coi, ">")[[1]]), 
                         equals(30))
})

                         
          


