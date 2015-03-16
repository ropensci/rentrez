context("elink")

elinks <- entrez_link(dbfrom = "pubmed", id = 19880848, db = "all")
message("entrez_link tests take some time, as they have to download many recs...")
commands <- c("neighbor_history", "neighbor_score", 
              "acheck", "ncheck", "lcheck", 
              "llinks", "llinkslib", "prlinks")


all_the_commands <- lapply(commands, function(cmd_arg)
    entrez_link(db="pubmed", dbfrom="pubmed", id=19880848, cmd=cmd_arg)
)

test_that("The record-linking funcitons work",{
    expect_that(elinks, is_a("elink"))
    expect_that(names(elinks$links), is_a("character"))
    expect_true(length(elinks$pubmed_nuccore) > 0)
})

test_that("Print elink behaves", {
    for(ret in all_the_commands){
        expect_output(elinks, "elink object with contents:\\s+\\$[A-Za-z]+")
    }
})

test_that("We detect missing ids from elink results",{
   expect_warning(
    entrez_link(dbfrom="pubmed", db="all", id=c(20203609,2020360999999,20203610))
   )
})
