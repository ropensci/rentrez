context("elink")

elinks <- entrez_link(dbfrom = "pubmed", id = 19880848, db = "all")

#
#We should maybe download these xmls and test the internal functions
# as these really take some downloading,... especially the lib. links?

message("(this may take some time, have to download many records)")
commands <- c("neighbor_history", "neighbor_score", 
              "acheck", "ncheck", "lcheck", 
              "llinks", "llinkslib", "prlinks")


all_the_commands <- lapply(commands, function(cmd_arg)
    entrez_link(db="pubmed", dbfrom="pubmed", id=19880848, cmd=cmd_arg)
)
test_that("The record-linking funcitons work",{
    expect_that(elinks, is_a("elink"))
    expect_that(names(elinks$links), is_a("character"))
    expect_true(length(elinks$links$pubmed_mesh_major) > 0)
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

test_that("Elink sub-elements can be acessed and printed", {
    expect_output(all_the_commands[[3]][[1]], 
                  "elink result with information from \\d+ databases")
    expect_output(all_the_commands[[8]]$linkouts[[1]],
                  "Linkout from [A-Za-z]+\\s+\\$Url")
})


test_that("Elink warns on mis-spelled/unknown cmds",{
    expect_warning(rcheck <- entrez_link(dbfrom = "pubmed",
                                         id = 19880848, db = "all", 
                                         cmd='rcheck'), 
                   "Command rcheck is not supported")
})
                   
