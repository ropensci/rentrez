#' Get summaries of objects in NCBI datasets from a unique ID 
#'
#' Contstructs a query from the given arguments, including a database name and
#' list of of unique IDs for that database then downloads the XML document 
#' created by that query. As the XML formats used by various NCBI databases 
#' differ from each other, no attempt is made to parse the xml file.
#' See the package-level documentation for general advice
#' on using the EUtils functions. 
#'
#'@export
#'@param db character Name of the database to search for
#'@param ids integer Id(s) for which data is being collected
#'@param \dots character Additional terms to add to the request 
#
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{xmlTreeParse}}
#' @examples
#'\dontrun{
#'  popset_summ <- entrez_summary(db="popset", ids=07082412)
#'  
#'}

entrez_summary <- function(db, ids, ...){
    args <- c(db=db, id=paste(ids, collapse=","), 
              email=entrez_email, tool=entrez_tool, ...)
    url_args <- paste(paste(names(args), args, sep="="), collapse="&")
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?retmod=xml"
    url_string <- paste(base_url, url_args, sep="&")
    record <- xmlTreeParse(getURL(url_string), useInternalNodes=TRUE)
    return(record)
}
