context("elink")

elinks_mixed <- entrez_link(dbfrom = "pubmed", id = c(19880848, 22883857), db = "all")
elinks_by_id <- entrez_link(dbfrom = "pubmed", id = c(19880848, 22883857), db = "all", by_id=TRUE)


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
    expect_that(elinks_mixed, is_a("elink"))
    expect_that(names(elinks_mixed$links), is_a("character"))
    expect_true(length(elinks_mixed$links$pubmed_mesh_major) > 0)
})


test_that("by_id mode works for elinks", {
    expect_that(elinks_by_id, is_a("elink_list"))
    expect_that(length(elinks_by_id), equals(2))
    expect_that(elinks_by_id[[1]], is_a("elink"))
})

test_that("elink printing behaves", {
    expect_output(elinks_by_id, "List of 2 elink objects,each containing")
    for(ret in all_the_commands){
        expect_output(ret, "elink object with contents:\\s+\\$[A-Za-z]+")
    }
})


# Removed following aparent change of policy by NCBI
# who no longer provide error message in this case
#test_that("We detect missing ids from elink results",{
#   expect_warning(
#    entrez_link(dbfrom="pubmed", db="all", id=c(20203609,2020360999999,20203610))
#   )
#})

test_that("Elink sub-elements can be acessed and printed", {
    expect_output(all_the_commands[[3]][[1]], 
                  "elink result with information from \\d+ databases")
    expect_output(all_the_commands[[8]]$linkouts[[1]],
                  "Linkout from [A-Za-z]+\\s+\\$Url")
})


test_that("Elink errors on mis-spelled/unknown cmds",{
    expect_error(rcheck <- entrez_link(dbfrom = "pubmed",
                                         id = 19880848, db = "all", 
                                         cmd='rcheck'))
})



