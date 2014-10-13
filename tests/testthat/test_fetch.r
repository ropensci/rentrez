context("fetching-records")
test_that("Functions to fetch records & summaries work", {
          #setup
          pop_ids = c("307082412", "307075396", "307075338", "307075274")
          pop_summ_xml <- entrez_summary(db="popset", 
                                         id=pop_ids, version="1.0")
          pop_summ_json <- entrez_summary(db="popset", 
                                          id=pop_ids, version="2.0")
          coi <- entrez_fetch(db = "popset", id = pop_ids[1], 
                              rettype = "fasta")

          #tests
          expect_that(pop_summ_xml, is_a("list"))
          expect_that(pop_summ_json, is_a("list"))

          expect_that(pop_summ_xml[[4]], is_a("esummary"))
          expect_that(pop_summ_json[[4]], is_a("esummary"))

          expect_that(length(pop_summ_xml[[1]]), is_more_than(12))
          expect_that(length(pop_summ_json[[1]]), is_more_than(12))

          #It would be nice to test whether the xml and json records
          # have the same data in them, but it turns out they don't
          # when they leave the NCBI, so let's ensure we can get some
          # info from each file, even if they won't be exactly the same
          sapply(pop_summ_xml, function(x)
                 expect_that(x[["Title"]], matches("Muraenidae")))
          sapply(pop_summ_json, function(x)
                 expect_that(x[["title"]], matches("Muraenidae")))
          
          #does fetching sequence records work?
          expect_that(length(strsplit(coi, ">")[[1]]), 
                         equals(30))
})


                         
          


