# test any parts of the README or tutorial that aren't already part of the test
# suite. Note, the final example of the README makes a lot calls to NCBI, so is
# not included here
context("documentation")

test_that("Examples in documentation work", {
    #setup (guarded so transient NCBI problems skip rather than fail)
    setup <- tryCatch({
        list(
            hox_paper = entrez_search(db="pubmed", term="10.1038/nature08789[doi]"),
            katipo_search = entrez_search(db="popset", term="Latrodectus katipo[Organism]")
        )
    }, error = function(e) skip(paste("NCBI not available:", conditionMessage(e))))

    expect_that(setup$hox_paper$ids, equals("20203609"))
    expect_true(setup$katipo_search$count >= 6)
})
