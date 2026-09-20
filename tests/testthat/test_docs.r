# test any parts of the README or tutorial that aren't already part of the test
# suite. Note, the final example of the README makes a lot calls to NCBI, so is
# not included here
context("documentation")

test_that("The pubmed example in the docs works", {
    hox_paper <- net(entrez_search(db="pubmed", term="10.1038/nature08789[doi]"))
    expect_that(hox_paper$ids, equals("20203609"))
})

#Kept separate from the pubmed example above. NCBI no longer serves popset to
#esearch, so gating both on the same skip took the pubmed assertion with it.
test_that("The popset example in the docs works", {
    skip_if_db_missing("popset")
    katipo_search <- net(entrez_search(db="popset",
                                       term="Latrodectus katipo[Organism]"))
    expect_true(katipo_search$count >= 6)
})
