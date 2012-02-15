#' Summarise an XML record from pubmed.
#'
#'
#'
#'@export
#'@param paper character the record to be parsed (as a character, 
#' expected to comed from \code{\link{entrez_fetch}})

parse_pubmed_xml <- function(paper){
 xml_paper <- xmlTreeParse(paper, useInternalNodes=TRUE)
 get_value <- function(path){
    return(xpathSApply(xml_paper, path, xmlValue))
 }
 res  <- list()
 res$title <- get_value("//ArticleTitle")
 res$authors <- paste(get_value("//Author/LastName"), get_value("//Author/ForeName"), sep=", ")
 res$year <- get_value("//PubDate/Year")
 res$journal <- get_value("//Journal/Title")
 res$volume <- get_value("//JournalIssue/Volume")
 res$issue <- get_value("//JournalIssue/Issue")
 res$pages <- get_value("//MedlinePgn")
 res$key_words <- get_value("//DescriptorName")
 res$doi <- get_value("//ArticleId[@IdType='doi']")
 res$pmid <- get_value("//ArticleId[@IdType='pubmed']")
 res$abstract <- get_value("//AbstractText")
 class(res) <- "pubmed_record"
 return(res) 
}

#' @S3method print pubmed_record
print.pubmed_record  <- function(record, ...){
 if(length(record$authors) == 1){
  display.author <- record$authors[1]
 }
 else if(length(record$authors) == 2){
  display.author <- with(record, paste(authors[1], authors[2], sep=". & "))
 }
 else
   display.author <- paste(record$authors[1], "et al")
   
 display <- with(rec, sprintf("%s. (%s). %s %s. %s: %s",
                      display.author, year, title, journal, volume, pages))
 cat(display)
 cat("\n")
}


