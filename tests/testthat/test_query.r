context("query")
test_that("Query building functions work", {

    #concatenate multiple IDs, include entrez terms
    query <- rentrez:::make_entrez_query("test", id=c(123,321))
    expect_that(query, matches("&id=123,321"))
    expect_that(query, matches("email=david.winter@gmail.com&tool=rentrez"))
    
    #"require one of" functions work
    expect_that(rentrez:::make_entrez_query("test", 
                                            x=10, 
                                            require_one_of=c("id", "db")), 
                throws_error())

    #should be able to give ints or characters to id and get a url
    q_char <- rentrez:::make_entrez_query("test", id=c("123", "321"))
    expect_that(query, matches("&id=123,321"))

    #specific function have right "require one of" settings
    expect_that(entrez_fetch(db="nuccore", rettype="fasta"), throws_error())
    expect_that(entrez_summary(db="nuccore"), throws_error())
    expect_that(entrez_link(db="nuccore", dbfrom="pubmed"), throws_error())
})



