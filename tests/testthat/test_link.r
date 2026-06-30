context("elink")

#setup (guarded so transient NCBI problems skip rather than abort the file)
message("(this may take some time, have to download many records)")
commands <- c("neighbor_history", "neighbor_score",
              "acheck", "ncheck", "lcheck",
              "llinks", "llinkslib", "prlinks")

ncbi_ok <- tryCatch({
    elinks_mixed <- entrez_link(dbfrom = "pubmed", id = c(19880848, 22883857), db = "all")
    elinks_by_id <- entrez_link(dbfrom = "pubmed", id = c(19880848, 22883857), db = "all", by_id=TRUE)
    all_the_commands <- lapply(commands, function(cmd_arg)
        entrez_link(db="pubmed", dbfrom="pubmed", id=19880848, cmd=cmd_arg)
    )
    TRUE
}, error = function(e) {
    message("NCBI not available: ", conditionMessage(e))
    FALSE
})

test_that("The record-linking funcitons work",{
    skip_if(!ncbi_ok, "NCBI not available")
    expect_that(elinks_mixed, is_a("elink"))
    expect_that(names(elinks_mixed$links), is_a("character"))
    expect_true(length(elinks_mixed$links$pubmed_mesh_major) > 0)
})


test_that("by_id mode works for elinks", {
    skip_if(!ncbi_ok, "NCBI not available")
    expect_that(elinks_by_id, is_a("elink_list"))
    expect_that(length(elinks_by_id), equals(2))
    expect_that(elinks_by_id[[1]], is_a("elink"))
})

test_that("elink printing behaves", {
    skip_if(!ncbi_ok, "NCBI not available")
    expect_output(print(elinks_by_id), "List of 2 elink objects,each containing")
    for(ret in all_the_commands){
        expect_output(print(ret), "elink object with contents:\\s+\\$[A-Za-z]+")
    }
})


test_that("We detect missing ids from elink results",{
   skip_if(!ncbi_ok, "NCBI not available")
   expect_warning(
    entrez_link(dbfrom="pubmed", db="all", id=c(20203609,2020360999999,20203610), by_id=TRUE)
   )
})

test_that("Elink sub-elements can be acessed and printed", {
    skip_if(!ncbi_ok, "NCBI not available")
    expect_output(print(all_the_commands[[3]][[1]]),
                  "elink result with information from \\d+ databases")
    expect_output(print(all_the_commands[[8]]$linkouts[[1]]),
                  "Linkout from [ A-Za-z]+\\s+\\$Url")
})


test_that("URls can be extracted from elink objs", {
   skip_if(!ncbi_ok, "NCBI not available")
   for(idx in 6:8){
       urls <- linkout_urls(all_the_commands[[idx]])
       expect_that(urls, is_a("list"))
       expect_that(urls[[1]], is_a("character"))
   }
})

test_that("Elink errors on mis-spelled/unknown cmds",{
    skip_if(!ncbi_ok, "NCBI not available")
    expect_error(rcheck <- entrez_link(dbfrom = "pubmed",
                                         id = 19880848, db = "all",
                                         cmd='rcheck'))
})
