#' Post IDs to Eutils for later use
#'
#'
#'
#'@export
#'@param db character Name of the database from which the IDs were taken
#'@param ids integer ID(s) for which data is being collected
#'@param \dots character Additional terms to add to the request 
#
#'@return QueryKey integer identifier for specific query in webhistory
#'@return WebEnv character identifier for session key to use with history
#' 
#' @examples
#'\dontrun{  
#' so_many_snails <- entrez_search(db="nuccore", "Gastropoda[Organism] AND COI[Gene]", retmax=200)
#' upload <- entrez_post(db="nuccore", ids=so_many_snails$ids)
#' cookie <- upload$WebEnv
#' first <- entrez_fetch(db="nuccore", ids="", file_format="fasta", WebEnv=cookie, query_key=upload$QueryKey, retend=100)
#' second <- entrez_fetch(db="nuccore", ids="", file_format="fasta", WebEnv=cookie, query_key=upload$QueryKey, retstart=100)
#'}

entrez_post <- function(db, ids, ...){
    args <- c(db=db, id=paste(ids, collapse=","), 
              email=entrez_email, tool=entrez_tool, ...)
    url_args <- paste(paste(names(args), args, sep="="), collapse="&")
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/epost.fcgi?"
    url_string <- paste(base_url, url_args, sep="&")
    record <- xmlTreeParse(getURL(url_string), useInternalNodes=TRUE)
    result <- xpathApply(record, "/ePostResult/*", xmlValue)
    names(result) <- c("QueryKey", "WebEnv")
    result$file <- record
    #NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(result)
}
