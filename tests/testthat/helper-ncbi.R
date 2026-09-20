# Live-API test helpers.
#
# Most rentrez tests call the live NCBI EUtils API, so a run can fail for two
# very different reasons: NCBI is unreachable or rate limiting, or rentrez
# itself is broken. Only the first is worth skipping over.
#
# Treating every error as "NCBI not available" would let a real regression pass
# CI. That matters here because throwing on an unexpected response is this
# package's most common bug (see #153, #172, #186, #187), so a guard that
# swallows errors would hide exactly the failures worth catching.

# Is this error NCBI or the network, rather than rentrez?
#
# curl raises its own condition class. rentrez turns HTTP status codes into
# plain errors in entrez_check(), so those are matched on the message. Only 429
# and the 5xx codes count as transient: any other 4xx means the request itself
# was wrong, which is a bug and should fail.
#
# The status pattern is anchored. entrez_check() pastes NCBI's reply body into
# the same message, and an unanchored match would let NCBI's own prose decide
# whether a rentrez bug gets reported.
is_network_error <- function(e) {
    if (inherits(e, "curl_error")) return(TRUE)
    msg <- conditionMessage(e)
    # An error rentrez raised from a status code. entrez_check() pastes NCBI's
    # reply body into the same string, so only the status at the front decides.
    # Matching anywhere would let NCBI's own prose classify a rentrez bug as a
    # network problem.
    if (grepl("^HTTP failure", msg)) {
        return(grepl("^HTTP failure:? *(429|5[0-9]{2})", msg))
    }
    # Anything else: a transport failure whose curl class did not survive.
    grepl(paste("Timeout was reached",
                "Operation timed out",
                "Could not resolve host",
                "Couldn't resolve host",
                "Failed to connect",
                "Connection refused",
                "Connection reset",
                "Recv failure",
                "Empty reply from server",
                "SSL connect error",
                sep = "|"),
          msg)
}

# Wrap a single live call inside a test. Skips when NCBI is unavailable and
# re-raises everything else, so a genuine failure is still reported as one.
net <- function(expr) {
    tryCatch(expr, error = function(e) {
        if (!is_network_error(e)) stop(e)
        testthat::skip(paste("NCBI not available:", conditionMessage(e)))
    })
}

# Error handler for the file-level setup blocks. Returns FALSE so that file's
# tests skip, but only when the cause was the network.
#
# The reason rides along on the value rather than going to message(), because
# testthat swallows a message raised at file level. A skip that reports only
# "NCBI not available" hides which call failed and why, which is the same fault
# as an error telling the reader to go and find a warning.
ncbi_setup_failed <- function(e) {
    if (!is_network_error(e)) stop(e)
    structure(FALSE, reason = paste("NCBI not available:", conditionMessage(e)))
}

# Skip the calling test when the file's setup did not run, reporting whatever
# the setup recorded. The reason arrives complete, so nothing is prefixed here.
skip_without_ncbi <- function(ok) {
    if (isTRUE(ok)) return(invisible(TRUE))
    reason <- attr(ok, "reason")
    testthat::skip(if (is.null(reason)) "NCBI not available: setup failed" else reason)
}

# The databases NCBI is serving right now, asked once per run.
#
# Tests name databases directly, and NCBI does withdraw them: popset went
# missing in 2026 (#214), and the genome database did the same and came back
# (#196). A test aimed at a database nobody is serving says nothing about
# rentrez, so it should skip rather than fail.
ncbi_dbs <- local({
    cached <- NULL
    function() {
        if (is.null(cached)) {
            cached <<- tryCatch(entrez_dbs(), error = function(e) NA_character_)
        }
        cached
    }
})

# For a file-level setup block. Returns TRUE when every named database is being
# served, and otherwise the same FALSE-carrying-a-reason that a failed setup
# returns, so the file's tests skip with something useful to read.
requires_dbs <- function(...) {
    dbs <- ncbi_dbs()
    if (identical(dbs, NA_character_)) return(TRUE)
    absent <- setdiff(c(...), dbs)
    if (!length(absent)) return(TRUE)
    structure(FALSE,
              reason = paste("NCBI is not serving:", paste(absent, collapse = ", ")))
}

# The same check from inside a test.
skip_if_db_missing <- function(...) {
    ok <- requires_dbs(...)
    if (!isTRUE(ok)) testthat::skip(attr(ok, "reason"))
    invisible(TRUE)
}
