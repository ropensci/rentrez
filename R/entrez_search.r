#' Search the NCBI databases using EUtils
#'
#' Contstructs a query with the given arguments, including a search term, and
#' a darabase name, then retrieves the XML document created by that query. 
#' See package-level documentation for general advice on using the Entrez functions 
#'
#'@export
#'@param db character Name of the database to search for
#'@param term character The search term
#'@param \dots character Additional terms to add to the request 
#'@param retmode character One of json (default) or xml. This will make no
#' difference in most cases.
#'@param config vector configuration options passed to httr::GET  
#'@seealso \code{\link[httr]{config}} for avaliable configs 
#
#'@return ids integer Unique IDS returned by the search
#'@return count integer Total number of hits for the search
#'@return retmax integer Maximum number of hits returned by the search
#'@return QueryKey integer identifier for specific query in webhistory
#'@return WebEnv character identifier for session key to use with history
#'@import  XML
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{xmlTreeParse}}
#' @examples
#' \dontrun{
#'    query <- "Gastropoda[Organism] AND COI[Gene]"
#'    web_env_search <- entrez_search(db="nuccore", query, usehistory="y")
#'    cookie <- web_env_search$WebEnv
#'    qk <- web_env_search$QueryKey 
#'    snail_coi <- entrez_fetch(db = "nuccore", WebEnv = cookie, query_key = qk,
#'                              file_format = "fasta", retmax = 10)
#'}

entrez_search <- function(db, term, config=NULL, retmode="xml", ... ){
    response <- make_entrez_query("esearch", 
                                  db=db, 
                                  term=term, 
                                  config=config,
                                  retmode=retmode, 
                                  ...)
    parsed <- parse_respone(response, retmode)
    parse_esearch(parsed)
}


#'@export
parse_esearch <- function(x) UseMethod("parse_esearch")
   
#'@export
parse_esearch.XMLInternalDocument <- function(x){
    res <- list( ids      = xpathSApply(x, "//IdList/Id", xmlValue),
                 count    = xpathSApply(x, "/eSearchResult/Count", xmlValue),
                 retmax   = xpathSApply(x, "/eSearchResult/RetMax", xmlValue),
                 QueryKey = xpathSApply(x, "/eSearchResult/QueryKey", xmlValue),
                 WebEnv   = xpathSApply(x, "/eSearchResult/WebEnv", xmlValue),
                 file     = x)
    res <- Filter(function(x) length(x) > 0, res)
    class(res) <- c("esearch", "list")
    return(res)
}

#'@export
parse_esearch.list <- function(x){
    res <- x$esearchresult[ c("idlist", "count", "retmax", "querykey", "webenv") ]
    names(res)[c(1,4,5)] <- c("ids", "QueryKey", "WebEnv")
    res <- Filter(function(x) !is.null(x), res)
    res$file <- x
    class(res) <- c("esearch", "list")
    return(res)
}

#'@export
print.esearch <- function(x, ...){
    msg<- paste("Entrez search result with", x$count, 
                "hits (object contains", length(x$ids), "IDs")
    if("WebEnv" %in% names(x)){
        cat(msg, "and a cookie)\n")
    }
    else{
        cat(msg, "and no cookie)\n")
    }
}

