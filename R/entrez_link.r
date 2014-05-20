#' Get links to datasets related to a unique ID from an NCBI database
#'
#' Contstructs a query with the given arguments and downloands the XML
#' document created by that query. Unique IDs from linked-databases
#'
#'@import RCurl XML
#'@export
#'@param db character Name of the database to search for links (or use "all" to earch all NCBI databases
#'@param dbfrom character Name of database from which the Id(s) orginate
#'@param \dots character Additional terms to add to the request
#
#'@return An elink object containing vectors of unique IDs
#' the vectors names take the form [db_from]_[db_to]
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{xmlTreeParse}}
#' @examples
#'
#'  pubmed_search <- entrez_search(db = "pubmed", term = "10.1016/j.ympev.2010.07.013[doi]")
#'  linked_data <- entrez_link(dbfrom = "pubmed", id = pubmed_search$ids, db = "all")
#' nucleotide_IDs <- linked_data$pubmed_nuccore
#'

entrez_link <- function(db, dbfrom, ...){
    url_string <- make_entrez_query("elink", db=db, dbfrom=dbfrom,
                                    require_one_of=c("id", "WebEnv"), ...)
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
