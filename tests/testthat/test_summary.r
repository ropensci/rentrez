context("fetching and parsing summary recs")


pop_ids = c("307082412", "307075396", "307075338", "307075274")
pop_summ_xml <- entrez_summary(db="popset", 
                               id=pop_ids, version="1.0")
pop_summ_json <- entrez_summary(db="popset", 
                                id=pop_ids, version="2.0")


test_that("Functions to fetch summaries work", {
          #tests
          expect_that(pop_summ_xml, is_a("list"))
          expect_that(pop_summ_json, is_a("list"))

          expect_that(pop_summ_xml[[4]], is_a("esummary"))
          expect_that(pop_summ_json[[4]], is_a("esummary"))
          sapply(pop_summ_json, function(x)
                 expect_that(x[["title"]], matches("Muraenidae"))
          )         
})  



test_that("List elements in XML are parsable", {
         rec <- entrez_summary(db="pubmed", id=25696867, version="1.0")
         expect_named(rec$History)
         expect_gt(length(rec$History), 0)
})
         

test_that("JSON and XML objects are similar", {
          #It would be nice to test whether the xml and json records
          # have the same data in them, but it turns out they don't
          # when they leave the NCBI, so let's ensure we can get some
          # info from each file, even if they won't be exactly the same
          sapply(pop_summ_xml, function(x)
                 expect_that(x[["Title"]], matches("Muraenidae")))
          sapply(pop_summ_json, function(x)
                 expect_that(x[["title"]], matches("Muraenidae")))
          
          expect_that(length(pop_summ_xml[[1]]), is_more_than(12))
          expect_that(length(pop_summ_json[[1]]), is_more_than(12))
          
})

test_that("We can print summary records", {
      expect_output(print(pop_summ_json), "List of  4 esummary records")        
      expect_output(print(pop_summ_json[[1]]), "esummary result with \\d+ items")        
      expect_output(print(pop_summ_xml), "List of  4 esummary records")        
      expect_output(print(pop_summ_xml[[1]]), "esummary result with \\d+ items")        
})

test_that("We can detect errors in esummary records", {
    expect_warning(
       entrez_summary(db="pmc", id=c(4318541212,4318541), version="1.0")
    )
    expect_warning(
       entrez_summary(db="pmc", id=c(4318541212,4318541))
    )
})
                         
test_that("We can extract elements from esummary object", {
    expect_that(extract_from_esummary(pop_summ_xml, c("Title", "TaxId")), is_a("matrix"))
    expect_that(extract_from_esummary(pop_summ_xml, c("Title", "TaxId"), simplify=FALSE), is_a("list"))
    expect_that(extract_from_esummary(pop_summ_json, "title"), is_a("character"))
   
})

test_that("We can extract elements from a single esummary", {
    expect_that(extract_from_esummary(pop_summ_xml[[1]], c("Title", "TaxId")), is_a("list"))
    expect_that(extract_from_esummary(pop_summ_xml[[1]], "Gi"), is_a("integer"))
    expect_that(extract_from_esummary(pop_summ_xml[[1]], "Gi", FALSE), is_a("list"))
})

test_that("We can get a list of one element if we ask for it", {
    expect_that(entrez_summary(db="popset", id=307075396, always_return_list=TRUE), is_a("list"))
    expect_that(entrez_summary(db="popset", id=307075396), is_a("esummary"))
})


test_that("We can fetch summaries on versioned sequences", {
    old_rec = entrez_summary(db="nuccore", id="AF123456.1")
    new_rec = entrez_summary(db="nuccore", id="AF123456.2")
    expect_match(old_rec$title, "testis-specific mRNA")
    expect_match(new_rec$title, "doublesex and mab-3 related transcription factor")    
})
