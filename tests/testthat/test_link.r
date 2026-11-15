context("elink")
skip_on_cran()

#setup (guarded so transient NCBI problems skip rather than abort the file)
message("(this may take some time, have to download many records)")
commands <- c("neighbor_history", "neighbor_score",
              "acheck", "ncheck", "lcheck",
              "llinks", "llinkslib", "prlinks")

#NCBI stops serving a cmd from time to time: llinkslib answers "command not
#supported" as of 2026 (#215). A cmd nobody serves says nothing about rentrez,
#so it is recorded here and the tests that need it skip. Results are held by
#name, because indexing this list by position breaks the moment it changes.
all_the_commands <- list()
unserved <- character()

ncbi_ok <- tryCatch({
    elinks_mixed <- entrez_link(dbfrom = "pubmed", id = c(19880848, 22883857), db = "all")
    elinks_by_id <- entrez_link(dbfrom = "pubmed", id = c(19880848, 22883857), db = "all", by_id=TRUE)
    for(cmd_arg in commands){
        res <- tryCatch(
            entrez_link(db="pubmed", dbfrom="pubmed", id=19880848, cmd=cmd_arg),
            error = function(e){
                #rentrez puts "NCBI message" in its 429 text too, so ask about
                #the network first. Rate limiting is not a retired cmd, and
                #recording it as one hides the rate limiting behind a green skip.
                if(is_network_error(e)) stop(e)
                if(!grepl("NCBI message", conditionMessage(e))) stop(e)
                NULL
            })
        if(is.null(res)) unserved <- c(unserved, cmd_arg)
        else all_the_commands[[cmd_arg]] <- res
    }
    TRUE
}, error = ncbi_setup_failed)

#Skip when NCBI is not currently serving any of the named cmds.
skip_without_cmds <- function(...){
    absent <- intersect(c(...), unserved)
    if(length(absent)){
        skip(paste("NCBI is not serving the elink cmd:", paste(absent, collapse=", ")))
    }
}

test_that("The record-linking funcitons work",{
    skip_without_ncbi(ncbi_ok)
    expect_that(elinks_mixed, is_a("elink"))
    expect_that(names(elinks_mixed$links), is_a("character"))
    #naming one category pins a detail NCBI can withdraw, which is how this
    #test broke: it asked for pubmed_mesh_major, and NCBI stopped serving it
    #(#219). What the test is named for is that links come back at all.
    expect_true(length(elinks_mixed$links) > 0)
    expect_true(sum(lengths(elinks_mixed$links)) > 0)
})


test_that("by_id mode works for elinks", {
    skip_without_ncbi(ncbi_ok)
    expect_that(elinks_by_id, is_a("elink_list"))
    expect_that(length(elinks_by_id), equals(2))
    expect_that(elinks_by_id[[1]], is_a("elink"))
})

test_that("elink printing behaves", {
    skip_without_ncbi(ncbi_ok)
    expect_output(print(elinks_by_id), "List of 2 elink objects,each containing")
    for(ret in all_the_commands){
        expect_output(print(ret), "elink object with contents:\\s+\\$[A-Za-z]+")
    }
})


test_that("We detect missing ids from elink results",{
   skip_without_ncbi(ncbi_ok)
   #NCBI answers for ids that do not exist, returning a LinkSet whose every
   #link is the query id pointing at itself, so nothing is left to detect and
   #no warning can fire (#220). Kept as a skip rather than deleted, because
   #the capability was real and the test is the record of it.
   skip("NCBI returns links for ids that do not exist, so none are missing (#220)")
   expect_warning(
    net(entrez_link(dbfrom="pubmed", db="all", id=c(20203609,2020360999999,20203610), by_id=TRUE))
   )
})

test_that("Elink sub-elements can be acessed and printed", {
    skip_without_ncbi(ncbi_ok)
    skip_without_cmds("acheck", "prlinks")
    expect_output(print(all_the_commands[["acheck"]][[1]]),
                  "elink result with information from \\d+ databases")
    expect_output(print(all_the_commands[["prlinks"]]$linkouts[[1]]),
                  "Linkout from [ A-Za-z]+\\s+\\$Url")
})


test_that("URls can be extracted from elink objs", {
   skip_without_ncbi(ncbi_ok)
   linkout_cmds <- setdiff(c("llinks", "llinkslib", "prlinks"), unserved)
   if(!length(linkout_cmds)) skip("NCBI is serving no linkout cmds")
   for(nm in linkout_cmds){
       urls <- linkout_urls(all_the_commands[[nm]])
       expect_that(urls, is_a("list"))
       expect_that(urls[[1]], is_a("character"))
   }
})

test_that("Elink errors on mis-spelled/unknown cmds",{
    skip_without_ncbi(ncbi_ok)
    expect_error(rcheck <- entrez_link(dbfrom = "pubmed",
                                         id = 19880848, db = "all",
                                         cmd='rcheck'))
})
