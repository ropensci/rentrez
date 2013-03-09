test_that("Functions to fetch records & summaries work", {
          #setup
          pop_ids = c("307082412", "307075396", "307075338", "307075274")
          pop_summ <- entrez_summary(db="popset", id=pop_ids)
          parsed_pop_summ <- xpathSApply(pop_summ, "//Item[@Name='Title']", 
                                         xmlValue)
          coi <- entrez_fetch(db = "popset", id = pop_ids[1], 
                              rettype = "fasta")

          #tests
          expect_that(pop_summ, is_a("XMLInternalDocument"))
          expect_that(parsed_pop_summ, is_a("character"))
          #expect_that(parsed_pop_summ, matches("Muraenidae"))
          sapply(parsed_pop_summ, expect_that, matches("Muraenidae"))

          expect_that(coi, is_a("character"))
          expect_that(length(strsplit(coi, ">")[[1]]), 
                         equals(30))
})

                         
          


