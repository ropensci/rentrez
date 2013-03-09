#' Get links to datasets related to an unique ID from an NCBI database
#'
#' Contstructs a url  with the given arguments, and downloads xml record
#' returned by that url. See the package-level documentation for general advice
#' on using the EUtils functions. 
#'
#'@export
#'@param db character Name of the database to search for
#'@param ids integer Id(s) for which data is being collected
#'@param \dots character Additional terms to add to the request 
#
#'@return A list containing vectords of unique IDs from different databases. 
#' the lists are name [db_from]_[db_to]
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{xmlTreeParse}}
#' @examples
#' 
#' pubmed_search <- entrez_search(db="pubmed", term="Dwarf Elephant", retmax=1)
#' other_data <- entrez_link(db="all", ids=pubmed_search$ids, dbfrom="pubmed")
#  other_data$pubmed_pubmed_reviews



entrez_link <- function(db, ids, ...){
    args <- c(db=db, id=paste(ids, collapse=","), 
              email=entrez_email, tool=entrez_tool, ...)
    url_args <- paste(paste(names(args), args, sep="="), collapse="&")
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?"
    url_string <- paste(base_url, url_args, sep="&")
    record <- xmlTreeParse(getURL(url_string), useInternalNodes=TRUE)
    db_names <- xpathSApply(record, "//LinkName", xmlValue)
    get_Ids <- function(dbname){
        path <-  paste("//LinkSetDb/LinkName[text()='", dbname, "']/../Link/Id", sep="")
        return(xpathSApply(record, path, xmlValue))
    }
    # Get ID from each database
    # Not simplified so if a single database get a named list (for consistancy)
    result <- sapply(db_names, get_Ids, simplify=FALSE) 
    result$file <- record
    class(result) <- c("elink", class(result))
    #NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(result)
}

#' @S3method print elink

print.elink <- function(x, ...){
   len <- length(x)
   cat(paste("elink result with ids from", len - 1, "databases:\n"))
   print (names(x)[-len])
}

