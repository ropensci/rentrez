context("elink by_id")

# In by_id mode the result should always be a list (one elink per id), even for
# a single id, and the call should not emit a spurious "invalid id" warning.
test_that("by_id=TRUE returns a one-element list with no spurious warning", {
    res <- tryCatch(
        expect_warning(
            entrez_link(db="protein", dbfrom="gene", id="93100", by_id=TRUE),
            NA),
        error = function(e) skip(paste("NCBI not available:", conditionMessage(e)))
    )
    expect_that(res, is_a("elink_list"))
    expect_equal(length(res), 1)
})
