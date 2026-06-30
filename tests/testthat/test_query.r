context("query")
test_that("Query building functions work", {

    setup <- tryCatch({
        list(
            #concatenate multiple IDs, include entrez terms
            q_int = rentrez:::make_entrez_query("efetch",
                                                db="nuccore",
                                                id=c(443610374, 443610372),
                                                config=list(),
                                                retmode="txt",
                                                rettype="fasta"),
            #should be able to give ints or characters to id and get a url
            q_chr = rentrez:::make_entrez_query("efetch",
                                                db="nuccore",
                                                id=c("443610374", "443610372"),
                                                retmode="txt",
                                                config=list(),
                                                rettype="fasta")
        )
    }, error = function(e) skip(paste("NCBI not available:", conditionMessage(e))))

    expect_equal(length(gregexpr(">", setup$q_int)[[1]]), 2)
    expect_equal(length(gregexpr(">", setup$q_chr)[[1]]), 2)

    #specific function have right "require one of" settings (local validation)
    expect_that(entrez_fetch(db="nuccore", rettype="fasta"), throws_error())
    expect_that(entrez_summary(db="nuccore", web_history="A", id=123), throws_error())
    expect_that(entrez_link(db="nuccore", dbfrom="pubmed"), throws_error())

    #httr passes on errors: a request to a non-existent eutil should error (404)
    expect_error(rentrez:::make_entrez_query("non-eutil",
                                             id=12,
                                             db="none",
                                             config=list()))
})


test_that("We give a useful error when an empty ID vector is passed", {
    ET <- tryCatch(
        entrez_search(db="taxonomy", term="Extraterrestrial[Organism]"),
        error = function(e) skip(paste("NCBI not available:", conditionMessage(e)))
    )
    expect_error(entrez_fetch(db="taxonomy", id= ET$ids, rettype="uilist"))
})
