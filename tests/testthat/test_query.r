context("query")
test_that("Query building functions work", {

    #concatenate multiple IDs, include entrez terms 
    query <- rentrez:::make_entrez_query("efetch", 
                                         db="nuccore", 
                                         id=c(443610374, 443610372),
                                         config=list(),
                                         retmode="txt",
                                         rettype="fasta")
    nrecs <- length(gregexpr(">", query)[[1]])
   
    expect_equal(nrecs, 2)
    

    #should be able to give ints or characters to id and get a url
    query <- rentrez:::make_entrez_query("efetch", 
                                         db="nuccore", 
                                         id=c("443610374", "443610372"),
                                         retmode="txt",
                                         config=list(),
                                         rettype="fasta")
    nrecs <- length(gregexpr(">", query)[[1]])
    expect_equal(nrecs, 2)

    #specific function have right "require one of" settings
    expect_that(entrez_fetch(db="nuccore", rettype="fasta"), throws_error())
    expect_that(entrez_summary(db="nuccore", web_history="A", id=123), throws_error())
    expect_that(entrez_link(db="nuccore", dbfrom="pubmed"), throws_error())

    #httr pases on errors
    #404
    expect_error(rentrez:::make_entrez_query("non-eutil", 
                                             id=12, 
                                             db="none",
                                             config=list()))
    #400
    expect_error(rentrez:::make_entrez_query("efetch", 
                                             id=1e-17,
                                             config=list(),  
                                             db="nuccore"))
    
})


test_that("We give a useful error when an empty ID vector is passed", {
    ET <- entrez_search(db="taxonomy", term="Extraterrestrial[Organism]")
    expect_error(entrez_fetch(db="taxonomy", id= ET$ids, rettype="uilist"))              
})
