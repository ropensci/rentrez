context("count-only search responses")

test_that("count-only search responses are parsed and printed", {
    xml <- XML::xmlParse(paste0(
        "<eSearchResult>",
        "<Count>42</Count>",
        "</eSearchResult>"
    ))
    json <- list(
        header = list(type = "esearch", version = "0.3"),
        esearchresult = list(count = "42")
    )

    xml_search <- parse_esearch(xml, history = FALSE)
    json_search <- parse_esearch(json, history = FALSE)

    expect_equal(xml_search$count, 42L)
    expect_equal(json_search$count, 42L)
    expect_named(xml_search, c("ids", "count", "retmax", "QueryTranslation", "file"))
    expect_length(xml_search$ids, 0)
    expect_true(is.na(xml_search$retmax))
    expect_true(is.na(xml_search$QueryTranslation))
    expect_equal(names(json_search), names(xml_search))
    fields <- c("ids", "count", "retmax", "QueryTranslation")
    expect_identical(json_search[fields], xml_search[fields])
    expect_output(print(xml_search), "Entrez search result with 42 hits")
    expect_output(print(json_search), "Entrez search result with 42 hits")
})

test_that("count-only json searches warn when history was requested", {
    json <- list(
        header = list(type = "esearch", version = "0.3"),
        esearchresult = list(count = "42")
    )

    expect_warning(
        res <- parse_esearch(json, history = TRUE),
        "no web history"
    )
    expect_null(res$web_history)
})
