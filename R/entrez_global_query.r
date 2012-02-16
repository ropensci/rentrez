#' See how many hits there are a given term across all NCBI Entrez databses
#'
#' Contstructs a url  with the given arguments, and downloads xml record
#' returned by that url. See the package-level documentation for general advice
#' on using the EUtils functions. 
#'
#'@export
#'@param term the search term to use
#
#'@return a named vector with counts for each a datbase
#' @examples
#' 
#' pubmed_search <- entrez_global_query(term="Dwarf Elephant")



entrez_global_query <- function(term, ...){
    args <- c(term=term,email=entrez_email, tool=entrez_tool, ...)
    url_args <- paste(paste(names(args), args, sep="="), collapse="&")
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/egquery.fcgi?retmode=xml"
    url_string <- paste(base_url, url_args, sep="&")
    record <- xmlTreeParse(url_string, useInternalNodes=TRUE, isURL=TRUE)
    db_names <- xpathSApply(record, "//ResultItem/DbName", xmlValue)
    get_Ids <- function(dbname){
        path <-  paste("//ResultItem/DbName[text()='", dbname, "']/../Count", sep="")
        return(xpathSApply(record, path, xmlValue))
    }
    return(sapply(db_names, get_Ids))
}
