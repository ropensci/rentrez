context("entrez_post")

prot_ids = c(15718680,157427902)

#setup (guarded so transient NCBI problems skip rather than abort the file)
ncbi_ok <- tryCatch({
    ret <- entrez_post(id=prot_ids, db="protein")
    TRUE
}, error = function(e) {
    message("NCBI not available: ", conditionMessage(e))
    FALSE
})

test_that("we can post ids", {
    skip_if(!ncbi_ok, "NCBI not available")
    qk <- ret$QueryKey
    expect_that(as.integer(qk), is_a("integer"))
    expect_false(is.na(as.integer(qk)))
    expect_that(ret$QueryKey, is_a("character"))
})

test_that("we can add to WebEnv", {
    skip_if(!ncbi_ok, "NCBI not available")
    ret2 <- net(entrez_post(id=119703751, db="protein", web_history=ret))
    first <- net(entrez_summary(db="protein", web_history=ret))
    second <- net(entrez_summary(db="protein", web_history=ret2))
    expect_equal(ret2$QueryKey, "2")
    expect_equal(ret2$WebEnv, ret$WebEnv)
    expect_equal(length(first), 2)
    expect_that(second, is_a("esummary"))#i.e. justone
})

test_that("Example works", {
     skip_if(!ncbi_ok, "NCBI not available")
     so_many_snails <- net(entrez_search(db="nuccore",
                           "Gastropoda[Organism] AND COI[Gene]", retmax=200))
     upload <- net(entrez_post(db="nuccore", id=so_many_snails$ids))
     first <- net(entrez_fetch(db="nuccore", rettype="fasta", web_history=upload, retstart=0, retmax=4))
     nrecs <-  length(gregexpr(">", first)[[1]])
     expect_equal(nrecs, 4)
})

test_that("We can print a post result", {
    skip_if(!ncbi_ok, "NCBI not available")
    expect_output(print(ret),
     "\\(QueryKey = \\d+, WebEnv = [A-Z0-9_a-z]+\\.\\.\\.\\)")
})
