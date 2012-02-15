#' Search the NCBI databases using EUtils
#'
#' Contstructs a url  with the given arguments, are downloads xml record
#' returned by that url. See the package-level documentation for general advice
#' on using the EUtils functions. 
#'
#'
#'@export
#'@param db character Name of the database to search for
#'@param term character The search term
#'@param \dots character Additional terms to add to the request 
#
#'@return ids integer Unique IDS returned by the search
#'@return count integer Total number of hits for the search
#'@return retmax integer Maximum number of hits returned by the search 
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{XML::xmlTreeParse}}
#' @examples
#' 
#' pubmed_search <- entrez_search(db="pubmed", term="Dwarf Elephant", retmax=1)
#' other_data <- entrez_link(db="all", ids=pubmed_search$ids, dbfrom="pubmed")


entrez_search <- function(db, term, ... ){
    args <- c(db=db, term=term, email=entrez_email, tool=entrez_tool, ...)
    url_args <- paste(paste(names(args), args, sep="="), collapse="&")
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?"
    search <- paste(base_url, url_args, sep="&")
    xml_result <- xmlParse(getURL(search))
    ids <- xpathSApply(xml_result, "//IdList/Id", xmlValue)
    count <- xpathSApply(xml_result, "/eSearchResult/Count", fun=xmlValue)
    retmax <- xpathSApply(xml_result, "/eSearchResult/RetMax", fun=xmlValue)
    return(list(file=xml_result, ids=as.integer(ids), 
                count=as.integer(count), 
                retmax=as.integer(retmax)
            ))
}
