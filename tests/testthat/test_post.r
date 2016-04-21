context("entrez_post")

prot_ids = c(15718680,157427902)
ret <- entrez_post(id=prot_ids, db="protein")

test_that("we can post ids", {
    qk <- ret$QueryKey
    expect_that(as.integer(qk), is_a("integer"))
    expect_false(is.na(as.integer(qk)))
    expect_that(ret$QueryKey, is_a("character"))
})

test_that("we can add to WebEnv", {
    ret2 <- entrez_post(id=119703751, db="protein", web_history=ret)
    first <- entrez_summary(db="protein", web_history=ret)
    second <- entrez_summary(db="protein", web_history=ret2)
    expect_equal(ret2$QueryKey, "2")
    expect_equal(ret2$WebEnv, ret$WebEnv)
    expect_equal(length(first), 2)
    expect_that(second, is_a("esummary"))#i.e. justone  
})

test_that("Example works", {
     so_many_snails <- entrez_search(db="nuccore", 
                           "Gastropoda[Organism] AND COI[Gene]", retmax=200)
     upload <- entrez_post(db="nuccore", id=so_many_snails$ids)
     first <- entrez_fetch(db="nuccore", rettype="fasta", web_history=upload, retstart=0, retmax=4)
     nrecs <-  length(gregexpr(">", first)[[1]])
     expect_equal(nrecs, 4)
})

test_that("We can print a post result", {
    expect_output(print(ret),
     "\\(QueryKey = \\d+, WebEnv = [A-Z0-9_]+\\.\\.\\.\\)") 
})
