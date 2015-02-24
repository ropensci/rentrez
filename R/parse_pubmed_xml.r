#' Summarise an XML record from pubmed.
#'
#'
#'@import XML
#'@export
#'@param raw_XML::xml character the record to be parsed (as a character, 
#' expected to come from \code{\link{entrez_fetch}})
#'@return Either a single pubmed_record object, or a list of several 
#'@examples
#' 
#' hox_paper <- entrez_search(db="pubmed", term="10.1038/nature08789[doi]")
#' hox_rel <- entrez_link(db="pubmed", dbfrom="pubmed", id=hox_paper$ids)
#' recs <- entrez_fetch(db="pubmed", 
#'                        id=hox_rel$pubmed_pubmed[1:3], 
#'                        rettype="XML::xml")
#' parse_pubmed_XML::xml(recs)
#'

parse_pubmed_xml<- function(raw_xml){
    parsed <- XML::xmlTreeParse(raw_xml, useInternalNodes=TRUE)
    res <- XML::xpathApply(parsed, 
                      "/PubmedArticleSet/PubmedArticle", 
                      parse_one_pubmed)
    if(length(res)==1){
        return(res[[1]])
    }
    class(res) <- c("multi_pubmed_record", "list")
    return(res)
}

#The work-horse function - get information from a single XML::xml rec
parse_one_pubmed <- function(paper){
    get_value <- function(path){
        return(XML::xpathSApply(paper, path, XML::xmlValue))
    }
    res  <- list()
    res$title <- get_value(".//ArticleTitle")
    res$authors <- paste(get_value(".//Author/LastName"),
                         get_value(".//Author/ForeName"), sep=", ")
    res$year <- get_value(".//PubDate/Year")
    res$journal <- get_value(".//Journal/Title")
    res$volume <- get_value(".//JournalIssue/Volume")
    res$issue <- get_value(".//JournalIssue/Issue")
    res$pages <- get_value(".//MedlinePgn")
    res$key_words <- get_value(".//DescriptorName")
    res$doi <- get_value(".//ArticleId[@IdType='doi']")
    res$pmid <- get_value(".//ArticleId[@IdType='pubmed']")
    res$abstract <- get_value(".//AbstractText")
    class(res) <- "pubmed_record"
 return(res) 
}



#' @export
print.pubmed_record  <- function(x, first_line=TRUE, ...){
 if(length(x$authors) == 1){
  display.author <- x$authors[1]
 }
 else if(length(x$authors) == 2){
  display.author <- with(x, paste(authors[1], authors[2], sep=". & "))
 }
 else
   display.author <- paste(x$authors[1], "et al")
   
 display <- with(x, sprintf(" %s. (%s).  %s. %s:%s",
                      display.author, year, journal, volume, pages))
 if(first_line){
     cat("Pubmed record", "\n")
 }
 cat(display, "\n")
}

#' @export
print.multi_pubmed_record <- function(x, ...){
    nrecs <- length(x)
    cat("List of", nrecs, "pubmed records\n")
    if( nrecs > 3){
        sapply(x[1:3], print, first_line=FALSE)
        cat(".\n.\n.\n")
    } else sapply(x[1:3], print, first_line=FALSE)
}
