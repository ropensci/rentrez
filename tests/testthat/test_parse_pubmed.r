context("pubmed xml parsing")

# Per the NLM PubMed XML DTD, ForeName is optional inside Author
# (<!ELEMENT Author (((LastName, ForeName?, Initials?, Suffix?) | CollectiveName), ...)>).
# Querying LastName and ForeName separately and calling paste() recycled the
# shorter ForeName vector across the wrong authors.

pubmed_xml <- function(authors_xml) {
    paste0(
        "<PubmedArticleSet><PubmedArticle><MedlineCitation>",
        "<PMID Version=\"1\">12345678</PMID>",
        "<Article>",
        "<Journal><JournalIssue><Volume>1</Volume><Issue>2</Issue>",
        "<PubDate><Year>2024</Year></PubDate></JournalIssue>",
        "<Title>Example Journal</Title></Journal>",
        "<ArticleTitle>Example title.</ArticleTitle>",
        "<Pagination><MedlinePgn>10-20</MedlinePgn></Pagination>",
        "<AuthorList>", authors_xml, "</AuthorList>",
        "</Article></MedlineCitation>",
        "<PubmedData><ArticleIdList>",
        "<ArticleId IdType=\"pubmed\">12345678</ArticleId>",
        "</ArticleIdList></PubmedData>",
        "</PubmedArticle></PubmedArticleSet>"
    )
}

test_that("parse_pubmed_xml pairs each author's LastName and ForeName independently", {
    xml <- pubmed_xml(paste0(
        "<Author><LastName>Madonna</LastName></Author>",
        "<Author><LastName>Smith</LastName><ForeName>Jane</ForeName></Author>",
        "<Author><LastName>Doe</LastName><ForeName>John</ForeName></Author>"
    ))
    rec <- parse_pubmed_xml(xml)
    expect_equal(rec$authors, c("Madonna", "Smith, Jane", "Doe, John"))
})

test_that("parse_pubmed_xml handles single author without ForeName and collective-only records", {
    single_xml <- pubmed_xml("<Author><LastName>Madonna</LastName></Author>")
    expect_equal(parse_pubmed_xml(single_xml)$authors, "Madonna")

    mixed_xml <- pubmed_xml(paste0(
        "<Author><CollectiveName>Study Group</CollectiveName></Author>",
        "<Author><LastName>Smith</LastName><ForeName>Jane</ForeName></Author>"
    ))
    expect_equal(parse_pubmed_xml(mixed_xml)$authors, "Smith, Jane")

    collective_only_xml <- pubmed_xml(
        "<Author><CollectiveName>Study Group</CollectiveName></Author>"
    )
    expect_equal(parse_pubmed_xml(collective_only_xml)$authors, character(0))
})
