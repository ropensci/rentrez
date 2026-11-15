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
#'@references \url{https://www.ncbi.nlm.nih.gov/books/NBK25499/#_chapter4_EPost_} 
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
    parse_epost(xmlTreeParse(response, useInternalNodes=TRUE))
}

#Read the web history out of an epost reply. NCBI reports a problem with the
#ids or the database in the body of an ordinary HTTP 200 reply, as <ERROR>
#nodes. They take the place of the QueryKey and WebEnv when NCBI has no
#history to return, and sit beside them when it has one and something to say
#about the ids, such as ids it left out. The two fields are read by name:
#read by position, the error text became the history (#227).
#
#When ids are posted to an existing history and NCBI can read none of them, it
#returns the history it was given, with an <ERROR>. It returns the same
#history, without one, for a list it has stored before. The reply does not
#say whether anything new went in, so the message goes out as a warning.
parse_epost <- function(x){
    query_key <- xpathSApply(x, "/ePostResult/QueryKey", xmlValue)
    web_env   <- xpathSApply(x, "/ePostResult/WebEnv", xmlValue)
    has_history <- length(query_key) == 1 && length(web_env) == 1 &&
                   nzchar(query_key) && nzchar(web_env)
    if(!has_history){
        #esearch_failure() quotes the <ERROR> nodes, which epost uses the same way
        stop(esearch_failure("EPost returned no web history", x), call.=FALSE)
    }
    check_xml_errors(x)
    result <- list(QueryKey = query_key, WebEnv = web_env)
    class(result) <- c("web_history", "list")
    result
}


