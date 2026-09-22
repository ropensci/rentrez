context("elink by_id")

# In by_id mode the result should always be a list (one elink per id), even for
# a single id, and the call should not emit a spurious "invalid id" warning.
test_that("by_id=TRUE returns a one-element list with no spurious warning", {
    skip_on_cran()
    #only the call is wrapped. A testthat failure is an error condition too, so
    #an expectation inside this tryCatch would be reported as a skip instead.
    seen <- NULL
    res <- withCallingHandlers(
        tryCatch(entrez_link(db="protein", dbfrom="gene", id="93100", by_id=TRUE),
                 error = function(e){
                     #only the network earns a skip here. A parser throwing is
                     #the bug this file exists to catch.
                     if(!is_network_error(e)) stop(e)
                     skip(paste("NCBI not available:", conditionMessage(e)))
                 }),
        warning = function(w){
            seen <<- c(seen, conditionMessage(w))
            invokeRestart("muffleWarning")
        })
    expect_null(seen)
    expect_s3_class(res, "elink_list")
    expect_equal(length(res), 1)
})

# by_id must not be a way around the guard that reports an NCBI error: a reply
# carrying no LinkSet is a failure whichever mode asked for it. No network.
test_that("by_id=TRUE still reports a reply that carried no LinkSet", {
    err <- XML::xmlTreeParse(
        "<eLinkResult><ERROR>Exception: llinkslib command not supported</ERROR></eLinkResult>",
        useInternalNodes = TRUE)
    for (mode in c(TRUE, FALSE)) {
        msg <- tryCatch(
            suppressWarnings(rentrez:::parse_elink(err, cmd = "neighbor",
                                                   by_id = mode, id = 1)),
            error = conditionMessage)
        expect_true(grepl("no LinkSet", msg, fixed = TRUE), info = paste("by_id =", mode))
        expect_true(grepl("llinkslib command not supported", msg, fixed = TRUE),
                    info = paste("by_id =", mode))
    }
})
