# test any parts of the README or tutorial that aren't already part of the test
# suite. Note, the final example of the README makes a lot calls to NCBI, so is
# not included here

test_that("Examples in documentation work", {
    #setup
    hox_paper <- entrez_search(db="pubmed", term="10.1038/nature08789[doi]")
    katipo_search <- entrez_search(db="popset", 
                                   term="Latrodectus katipo[Organism]")
    


    expect_that(hox_paper$ids, equals("20203609"))
    expect_true(katipo_search$count >= 6)
})

    
