context("live API reachable")

# The rest of the suite skips its tests when NCBI is unavailable. That keeps a
# bad day at NCBI from failing the build, but it also means a run in which every
# test skipped would otherwise report success while having checked nothing.
#
# This test makes the difference visible: it retries a few times, and fails if
# NCBI never answers. A build that could not verify anything is not a passing
# build.
test_that("NCBI is reachable", {
    res <- NULL
    for (attempt in 1:3) {
        res <- tryCatch(entrez_search(db = "pubmed", term = "Homo sapiens[Organism]"),
                        error = function(e) NULL)
        if (!is.null(res)) break
        Sys.sleep(5)
    }
    expect_false(is.null(res),
                 info = "no live NCBI call succeeded, so this run verified nothing")
    expect_true(inherits(res, "esearch"))
    expect_true(res$count > 0)
})
