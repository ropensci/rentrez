#' Get summaries of objects in NCBI datasets from a unique ID 
#'
#' Contstructs a query from the given arguments, including a database name and
#' list of of unique IDs for that database then downloads the XML document 
#' created by that query. The XML document is parsed, with the 
#'
#'@export
#'@param db character Name of the database to search for
#'@param \dots character Additional terms to add to the request. Requires either
#'   ID (unique id(s) for records in a given database) or WebEnv (a character
#'   containing a cookie created by a previous entrez query).
#'@return A list of esummary records (if multiple IDs are passed) or a single
#' record.
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{xmlTreeParse}}
#' @examples
#'\dontrun{
#'  pop_ids = c("307082412", "307075396", "307075338", "307075274")
#'  pop_summ <- entrez_summary(db="popset", id=pop_ids)
#'  sapply(popset_summ, "[[", "Title")
#'}

entrez_summary <- function(db, ...){
    url_string <- make_entrez_query("esummary", db=db,
                                    require_one_of=c("id", "WebEnv"), ...)
    whole_record <- xmlTreeParse(getURL(url_string), useInternalNodes=TRUE)
    rec <- lapply(whole_record["//DocSum"], parse_esummary)
    if(length(rec) == 1){
        return(rec[[1]])
    }
    return(rec)
}

#' @S3method print esummary

print.esummary <- function(x, ...){
    len <- length(x)
    cat(paste("esummary result with", len - 1, "items:\n"))
    print(names(x)[-len])
}



# Prase a sumamry XML 
#
# Logic goes like this
# 1. Define functions parse_esumm_* to handle all data types
# 2. For each node detect type, parse accordingly
# 3. wrap it all up in function parse_esummary that 
#
#

parse_esummary <- function(record){
    res <- xpathApply(record, "//DocSum/Item", parse_node)
    names(res) <- xpathApply(record, "//DocSum/Item", xmlGetAttr, "Name")
    res <- c(res, file=record)
    class(res) <- c("esummary", class(res))
    return(res)
}

parse_node <- function(node) {
    node_type <- xmlGetAttr(node, "Type")
    node_fxn <- switch(node_type, 
                       "Integer" = parse_esumm_int,
                       "List" = parse_esumm_list,
                       "Structure" = parse_esumm_list,
                       xmlValue) #unnamed arguments to switch = default val.
    return(node_fxn(node))

}

parse_esumm_int <- function(node) as.integer(xmlValue(node))

parse_esumm_list <- function(node){
    res <- lapply(node["Item"], parse_node)
    names(res) <- lapply(node["Item"], xmlGetAttr, "Name")
    return(res)
}

parse_esumm_struct <- function(node){
    res <- la

