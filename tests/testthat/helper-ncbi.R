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
is_network_error <- function(e) {
    inherits(e, "curl_error") ||
        grepl(paste("HTTP failure:? *(429|5[0-9]{2})",
                    "Timeout was reached",
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
              conditionMessage(e))
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
ncbi_setup_failed <- function(e) {
    if (!is_network_error(e)) stop(e)
    message("NCBI not available: ", conditionMessage(e))
    FALSE
}
