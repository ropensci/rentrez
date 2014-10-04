#' Get infromation about EUtils databases
#
#' Contstructs a query to NCBI's einfo and return parsed XML object
#' Note The most common uses for einfo API are to list fields against which
#' a database can be searched, or linked databases avaliabl,
#' @param db characater database about which to retreive information (optional,
#' @param config config vector passed on to httt::GET
#' @return XMLInternalDocument with information describing either all the
#' databases avaliable in Eutils (if db is not set) or one pariticular database
#' (set by 'db')
#' @seealso \code{\link[httr]{config}} for avaliable httr configurations
#' @seealso \code{\link{entrez_linked_databases}}
#' @seealso \code{\link{entrez_search_fields}}
#' @examples
#' all_the_data <- entrez_info()
#' xpathSApply(all_the_data, "//DbName", xmlValue)
#' @export

entrez_info <- function(db=NULL, config=NULL){
    response <- make_entrez_query("einfo", db=db, config=config)
    return(xmlTreeParse(response, useInternalNodes=TRUE))
}


entrez_avaliable_dbs <- function(config=NULL){
    xpathSApply(entrez_info(config), "//DbName", xmlValue)
}

#'@export
entrez_linked_dbs <- function(db, config=NULL){
    unparsed <- xpathApply(entrez_info(db, config), "//Link", xmlChildren)
    res <- lapply(unparsed, parse_einfo_link)
    names(res) <- sapply(res, "[[", "Name")
    return(res)


}


parse_einfo_link <- function(x){
    res <- lapply(x,xmlValue)
    class(res) <- c("eInfoLink", "link")
    res
}
#'@export
print.eInfoLink <- function(x, ...){
 cat("\tdb_to = ", x$DbTo, "\n\t", x$Description, "\n")
}








