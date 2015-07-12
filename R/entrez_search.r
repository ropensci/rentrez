#' Search the NCBI databases using EUtils
#'
#' Constructs a query with the given arguments, including a search term, and
#' a database name, then retrieves the XML document created by that query. 
#' 
#'@export
#'@param db character Name of the database to search for
#'@param term character The search term
#'@param use_history locgical Return a web_history object for use in 
#' later calls to the NCBI
#'@param retmode character One of json (default) or xml. This will make no
#' difference in most cases.
#'@param config vector configuration options passed to httr::GET  
#'@param \dots character Additional terms to add to the request 
#'@seealso \code{\link[httr]{config}} for available configs 
#'@seealso \code{\link{entrez_db_searchable}} to get a set of search fields that
#' can be used in \code{term} for any base
#'@return ids integer Unique IDS returned by the search
#'@return count integer Total number of hits for the search
#'@return retmax integer Maximum number of hits returned by the search
#'@return web_history A web_history object for use in subsequent calls to NCBI
#'@return QueryTranslation character, search term as the NCBI interpreted it
#'@return file either and XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link[XML]{xmlTreeParse}} or, if \code{retmode} was set to json a list
#' resulting from the returned JSON file being parsed with
#' \code{\link[jsonlite]{fromJSON}}.
#' @examples
#' \dontrun{
#'    query <- "Gastropoda[Organism] AND COI[Gene]"
#'    web_env_search <- entrez_search(db="nuccore", query, usehistory="y")
#'    cookie <- web_env_search$WebEnv
#'    qk <- web_env_search$QueryKey 
#'    snail_coi <- entrez_fetch(db = "nuccore", WebEnv = cookie, query_key = qk,
#'                              file_format = "fasta", retmax = 10)
#'}
#'\donttest{
#' 
#' fly_id <- entrez_search(db="taxonomy", term="Drosophila")
#' #Oh, right. There is a genus and a subgenus name Drosophila...
#' #how can we limit this search
#' (tax_fields <- entrez_db_searchable("taxonomy"))
#' #"RANK" loots promising
#' tax_fields$RANK
#' entrez_search(db="taxonomy", term="Drosophila & Genus[RANK]")
#'}

entrez_search <- function(db, term, config=NULL, retmode="xml", use_history=FALSE, ... ){
    usehistory <- if(use_history) "y" else "n"
    response <- make_entrez_query("esearch", 
                                  db=db, 
                                  term=term, 
                                  config=config,
                                  retmode=retmode, 
                                  usehistory=usehistory,
                                  ...)
    parsed <- parse_response(response, retmode)
    parse_esearch(parsed, history=use_history)
}


parse_esearch <- function(x, history) UseMethod("parse_esearch")
   
parse_esearch.XMLInternalDocument <- function(x, history){
    res <- list( ids      = xpathSApply(x, "//IdList/Id", xmlValue),
                 count    = xpathSApply(x, "/eSearchResult/Count", xmlValue),
                 retmax   = xpathSApply(x, "/eSearchResult/RetMax", xmlValue),
                 QueryTranslation   = xpathSApply(x, "/eSearchResult/QueryTranslation",xmlValue),
                 file     = x)
    if(history){
        res$web_history = web_history(
          QueryKey = xpathSApply(x, "/eSearchResult/QueryKey", xmlValue),
          WebEnv   = xpathSApply(x, "/eSearchResult/WebEnv", xmlValue)
        )
    }
    class(res) <- c("esearch", "list")
    return(res)
}

parse_esearch.list <- function(x, history){
    res <- x$esearchresult[ c("idlist", "count", "retmax", "querytranslation") ]
    names(res)[c(1,4)] <- c("ids", "QueryTranslation")
    if(history){
        res$web_history = web_history(QueryKey = x$esearch_result[["querykey"]], 
                                      WebEnv   = x$esearch_result[["webenv"]])
    }
    res$file <- x
    class(res) <- c("esearch", "list")
    return(res)
}

#'@export
print.esearch <- function(x, ...){
    display_term <- if(nchar(x$QueryTranslation) > 50){
        paste(substr(x$QueryTranslation, 1, 50), "...")
    } else x$QueryTranslation
    cookie_word <- if("web_history" %in% names(x)) "a" else "no"
    msg<- paste("Entrez search result with", x$count, "hits (object contains",
                length(x$ids), "IDs and", cookie_word, 
                "web_history object)\n Search term (as translated): "  , display_term, "\n")
    cat(msg)
}
 
