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
#'@param id integer Id(s) for which data is being collected
#'@param \dots character Additional terms to add to the request 
#
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{xmlTreeParse}}
#' @examples
#'\dontrun{
#'  popset_summ <- entrez_summary(db="popset", id=07082412)
#'  
#'}

entrez_summary <- function(db, ...){
    url_string <- make_entrez_query("esummary", db=db,
                                    require_one_of=c("id", "WebEnv"), ...)
    record <- xmlTreeParse(getURL(url_string), useInternalNodes=TRUE)
    return(record)
}
