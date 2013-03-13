#' Get summaries of objects in NCBI datasets from a unique ID 
#'
#' Contstructs a query from the given arguments, including a database name and
#' list of of unique IDs for that database then downloads the XML document 
#' created by that query. As the XML formats used by various NCBI databases 
#' differ from each other, no attempt is made to parse the xml file.
#' See the package-level documentation for general advice
#' on using the EUtils functions. 
#'
#'@export
#'@param db character Name of the database to search for
#'@param \dots character Additional terms to add to the request 
#
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{xmlTreeParse}}
#' @examples
#'\dontrun{
#'  popset_summ <- entrez_summary(db="popset", id=07082412)
#'  
#'}

entrez_summary <- function(db, ...){
    url_string <- make_entrez_query("esummary", db=db,
                                    require_one_of=c("id", "WebEnv"), ...)
    record <- xmlTreeParse(getURL(url_string), useInternalNodes=TRUE)
    return(parse_esummary(record))
}


# Prase a sumamry XML 
#
# For each node the logic goes like this
# 1. Find the "Type" of a node
# 2. Convert to appropriate R object w/ functions hased in 'fun hash'
# 3. Return the whole thing as a named list
#
# usage
# snp_sum <- rentrez::entrez_summary(db="snp", id=4312)
# parse_esummary(snp_sum)
# 

parse_esummary <- function(record){
    res <- xpathApply(record, "//DocSum/Item", parse_node)
    names(res) <- xpathApply(record, "//DocSum/Item", xmlGetAttr, "Name")
    res <- c(res, file=record)
    class(res) <- c("esummary", class(res))
    return(res)
}

parse_node <- function(node) {
    fxn_hash <- c(
                  "String" = xmlValue,
                  "Integer" = parse_esumm_int,
                  "Structure" = parse_esumm_list,
                  "List" = parse_esumm_list)
    node_fxn <- fxn_hash[[xmlGetAttr(node, "Type")]]
    return(node_fxn(node))

}

parse_esumm_int <- function(node) as.integer(xmlValue(node))

parse_esumm_list <- function(node){
    res <- lapply(node["Item"], parse_summary)
    names(res) <- lapply(node["Item"], xmlGetAttr, "Name")
    return(res)
}

