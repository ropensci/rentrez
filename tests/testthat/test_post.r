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
    ret2 <- entrez_post(id=119703751, db="protein", WebEnv=ret$WebEnv)
    first <- entrez_summary(db="protein", WebEnv=ret2$WebEnv, query_key=1)
    second <- entrez_summary(db="protein", WebEnv=ret2$WebEnv, query_key=2)
    expect_equal(length(first), 2)
    expect_that(second, is_a("esummary")) 
})

test_that("Example works", { 
     so_many_snails <- entrez_search(db="nuccore", 
                           "Gastropoda[Organism] AND COI[Gene]", retmax=200)
     upload <- entrez_post(db="nuccore", id=so_many_snails$ids)
     cookie <- upload$WebEnv
     first <- entrez_fetch(db="nuccore", rettype="fasta", WebEnv=cookie,
                           query_key=upload$QueryKey, retstart=0, retmax=4)
     nrecs <-  length(gregexpr(">", first)[[1]])
     expect_equal(nrecs, 4)
})
     
