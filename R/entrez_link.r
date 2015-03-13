#' Get links to datasets related to a unique ID from an NCBI database
#'
#' Constructs a query with the given arguments and downloads the XML
#' document created by that query. 
#'
#'@import XML
#'@export
#'@param db character Name of the database to search for links (or use "all" to 
#' search all databases available for \code{db}. \code{entrez_db_links} allows you
#' to discover databases that might have linked information (see examples).
#'@param dbfrom character Name of database from which the Id(s) originate
#'@param \dots character Additional terms to add to the request
#'@param config vector configuration options passed to httr::GET  
#'@seealso \code{\link[httr]{config}} for available configs 
#'@return An elink object containing vectors of unique IDs
#' the vectors names take the form [db_from]_[db_to]
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{xmlTreeParse}}
#' @examples
#' \donttest{
#'  (pubmed_search <- entrez_search(db = "pubmed", term ="10.1016/j.ympev.2010.07.013[doi]"))
#'  (linked_dbs <- entrez_db_links("pubmed"))
#'  nucleotide_data <- entrez_link(dbfrom = "pubmed", id = pubmed_search$ids, db ="nuccore")
#'  #All the links
#'  entrez_link(dbfrom="pubmed", db="all", id=pubmed_search$ids)
#'}
#'


entrez_link <- function(db, dbfrom, config=NULL, ...){
    response <- make_entrez_query("elink", db=db, dbfrom=dbfrom,
                                  config=config, ..., 
                                  require_one_of=c("id", "WebEnv"))
    record <- XML::xmlTreeParse(response, useInternalNodes=TRUE)
    search_ids <- XML::xpathSApply(record, "//IdList/Id", 
                                   function(x) as.numeric(XML::xmlValue(x))
    )
    if(-1 %in% search_ids){
       msg <- paste("Some  IDs not found in", dbfrom)
       warning(msg)
    }
    db_names <- XML::xpathSApply(record, "//LinkName", XML::xmlValue)

    # Get ID from each database
    # Not simplified so if a single database get a named list (for consistancy)
    result <- sapply(db_names, get_linked_ids, record=record, simplify=FALSE)
    result$file <- record
    class(result) <- c("elink", class(result))
    #NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(result)
}

#' @export

print.elink <- function(x, ...){
   len <- length(x)
   cat(paste("elink result with ids from", len - 1, "databases:\n"))
   print (names(x)[-len], quote=FALSE)
}

get_linked_ids <- function(record, dbname){
    path <-  paste("//LinkSetDb/LinkName[text()='", dbname, "']/../Link/Id", sep="")
    return(XML::xpathSApply(record, path, XML::xmlValue))
}
