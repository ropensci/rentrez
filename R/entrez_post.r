#' Post IDs to Eutils for later use
#'
#'
#'
#'@export
#'@param db character Name of the database from which the IDs were taken
#'@param id integer ID(s) for which data is being collected
#'@param \dots character Additional terms to add to the request 
#
#'@return QueryKey integer identifier for specific query in webhistory
#'@return WebEnv character identifier for session key to use with history
#'@import RCurl XML
#'
#' @examples
#'\dontrun{  
#' so_many_snails <- entrez_search(db="nuccore", 
#'                       "Gastropoda[Organism] AND COI[Gene]", retmax=200)
#' upload <- entrez_post(db="nuccore", id=so_many_snails$ids)
#' cookie <- upload$WebEnv
#' first <- entrez_fetch(db="nuccore", file_format="fasta", WebEnv=cookie,
#'                       query_key=upload$QueryKey, retend=10)
#' second <- entrez_fetch(db="nuccore", file_format="fasta", WebEnv=cookie,
#'                        query_key=upload$QueryKey, retstart=10)
#'}

entrez_post <- function(db, id, ...){
    url_string <- make_entrez_query("epost", db=db,id=id, ...)
    record <- xmlTreeParse(getURL(url_string), useInternalNodes=TRUE)
    result <- xpathApply(record, "/ePostResult/*", xmlValue)
    names(result) <- c("QueryKey", "WebEnv")
    result$file <- record
    #NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(result)
}
