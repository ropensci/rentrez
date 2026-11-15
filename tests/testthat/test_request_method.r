context("choosing GET or POST")

# make_entrez_query() sends a request by GET unless it carries more than 200
# ids or its url would be too long for NCBI (#163). debug_mode returns the
# request it would send, with the method it chose. No network needed.
method_for <- function(...) {
    rentrez:::make_entrez_query(..., config = list(), debug_mode = TRUE)$method
}

pmids <- function(n) 30000000L + seq_len(n)
accessions <- function(n) sprintf("NM_%09d.1", seq_len(n))
long_term <- paste0("word", 1:300, "[Title/Abstract]", collapse = " OR ")

test_that("an ordinary request goes by GET", {
    expect_equal(method_for("esearch", db = "pubmed", term = "Cancer"), "GET")
})

# 200 PubMed ids fit in a url that NCBI accepts, and an error from a GET
# request can quote its query, so the length test must not move them to POST.
test_that("200 ids stay on GET and 201 go by POST", {
    expect_equal(method_for("esummary", db = "pubmed", id = pmids(200)), "GET")
    expect_equal(method_for("esummary", db = "pubmed", id = pmids(201)), "POST")
})

# The search term in #163 was 3893 characters long. A list of 200 ids or fewer
# makes a long url too when the ids are accessions.
test_that("a request too long for a url goes by POST", {
    expect_equal(method_for("esearch", db = "pubmed", term = long_term), "POST")
    expect_equal(method_for("efetch", db = "nuccore", id = accessions(200)), "POST")
})

# The length that counts is the escaped one: 1200 brackets are 1200 characters
# as typed and 3600 in a url.
test_that("the url is measured after escaping", {
    brackets <- paste(rep("[", 1200), collapse = "")
    expect_equal(method_for("esearch", db = "pubmed", term = brackets), "POST")
})

test_that("the cut-off is the length of the url", {
    uri <- "https://example.org/?"
    padding <- function(n) list(a = paste(rep("a", n), collapse = ""))
    longest <- rentrez:::max_url_length() - nchar(paste0(uri, "a="))
    expect_equal(rentrez:::request_method(uri, padding(longest), 0, FALSE), "GET")
    expect_equal(rentrez:::request_method(uri, padding(longest + 1), 0, FALSE), "POST")
})

# A by_id request writes its ids into the url itself, so POST as it stands
# would not shorten that url. Its length is left out of the choice.
test_that("the length of a by_id request does not move it to POST", {
    expect_equal(method_for("elink", dbfrom = "nuccore", db = "gene",
                            id = accessions(180), by_id = TRUE), "GET")
})

# The tests above read the choice from debug_mode, which returns before any
# request is made. This one swaps httr's GET and POST for stand-ins, to see
# which of them make_entrez_query() calls and where it puts the fields.
test_that("the request is sent by the chosen method", {
    stand_in <- function(verb) function(url, ..., query = NULL, body = NULL, config = list()) {
        fields <- if (is.null(body)) "url" else "body"
        structure(list(
            status_code = 200L,
            url = url,
            headers = structure(list(`content-type` = "text/plain"),
                                class = c("insensitive", "list")),
            content = charToRaw(paste(verb, "with the fields in the", fields))),
            class = "response")
    }
    local_mocked_bindings(GET = stand_in("GET"), POST = stand_in("POST"),
                          .package = "httr")
    send <- function(term) {
        rentrez:::make_entrez_query("esearch", config = NULL, db = "pubmed", term = term)
    }
    expect_equal(send("Cancer"), "GET with the fields in the url")
    expect_equal(send(long_term), "POST with the fields in the body")
})
