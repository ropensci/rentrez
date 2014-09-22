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
    
    #"require one of" functions work
    expect_that(rentrez:::make_entrez_query("test", 
                                            x=10, 
                                            config=list(),
                                            require_one_of=c("id", "db")), 
                throws_error())

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
    expect_that(entrez_summary(db="nuccore"), throws_error())
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



