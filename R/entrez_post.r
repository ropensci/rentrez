#' Post IDs to Eutils for later use
#'
#'
#'
#'@export
#'@param db character Name of the database from which the IDs were taken
#'@param id vector with unique ID(s) for records in database \code{db}. 
#'@param web_history A web_history object. Can be used to add to additional
#' identifiers to an existing web environment on the NCBI 
#'@param \dots character Additional terms to add to the request, see NCBI
#'documentation linked to in references for a complete list
#'@param config vector of configuration options passed to httr::GET  
#'@references \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#_chapter4_EPost_} 
#'@seealso \code{\link[httr]{config}} for available httr configurations
#'@importFrom XML xmlTreeParse
#' @examples
#'\dontrun{  
#' so_many_snails <- entrez_search(db="nuccore", 
#'                       "Gastropoda[Organism] AND COI[Gene]", retmax=200)
#' upload <- entrez_post(db="nuccore", id=so_many_snails$ids)
#' first <- entrez_fetch(db="nuccore", rettype="fasta", web_history=upload,
#'                       retmax=10)
#' second <- entrez_fetch(db="nuccore", file_format="fasta", web_history=upload,
#'                        retstart=10, retmax=10)
#'}

entrez_post <- function(db, id=NULL, web_history=NULL, config=NULL, ...){
    args  <-list("epost", db=db, config=config, id=id, web_history=web_history, ...)
    if(!is.null(web_history)){
        args <- c(args, WebEnv=web_history$WebEnv, query_key = web_history$QueryKey)
        args$web_history <- NULL
    }
    response  <- do.call(make_entrez_query, args)
    record <- xmlTreeParse(response, useInternalNodes=TRUE)
    result <- xpathApply(record, "/ePostResult/*", XML::xmlValue)
    names(result) <- c("QueryKey", "WebEnv")
    class(result) <- c("web_history", "list")
    #NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(result)
}


