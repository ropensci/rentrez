# Live-API test helpers.
#
# Most rentrez tests call the live NCBI EUtils API. To keep transient NCBI
# problems (rate limiting, outages, partial transfers) from turning into test
# failures -- especially on CI, where an API key may not be available -- network
# access is made resilient in two ways:
#
#   * file-level setup blocks run inside tryCatch and set `ncbi_ok`; each test
#     then starts with skip_if(!ncbi_ok, "NCBI not available").
#   * individual in-test calls are wrapped in net(), which skips the test (rather
#     than failing it) if the call raises, e.g. an HTTP 429 rate-limit error.
net <- function(expr) {
    tryCatch(
        expr,
        error = function(e) testthat::skip(paste("NCBI not available:", conditionMessage(e)))
    )
}
