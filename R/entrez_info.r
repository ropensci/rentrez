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


entrez_dbs <- function(config=NULL){
    xpathSApply(entrez_info(config), "//DbName", xmlValue)
}

#'@export
entrez_db_links <- function(db, config=NULL){
    unparsed <- xpathApply(entrez_info(db, config), "//Link", xmlChildren)
    res <- lapply(unparsed, lapply, xmlValue)
    names(res) <- sapply(res, "[[", "Name")
    class(res) <- c("eInfoLink", "eInfoList", "list")
    return(res)
}

#'@export
entrez_db_summary <- function(db, config=NULL){
    rec <- entrez_info(db, config)
    unparsed <- xpathApply( rec, "//DbInfo/*[not(self::LinkList or self::FieldList)]")
    res <- sapply(unparsed, xmlValue)
    names(res) <- sapply(unparsed, xmlName)
    res
}

entrez_db_searchable <- function(db, config=NULL){
    rec <- entrez_info(db, config)
    unparsed <- xpathApply(rec, 
                           "/eInfoResult/DbInfo/FieldList/Field",
                           xmlChildren)
    res <- lapply(unparsed, lapply, xmlValue)
    names(res) <- sapply(res, "[[", "Name")
    class(res) <- c("eInfoSearch", "eInfoList", "list")
    res
}

print_maker <- function(x, name){
    function(x, ...){
        cat(name, "result with the following fields:\n")
        print(names(x))
    }
}

#'@export
print.eInfoSearch <- print_maker(x, "Search fields")

#'@export
print.eInfoLink <-  print_maker(x, "Linked dbs")

#'@export
as.data.frame.eInfoList <- function(x, ...){
    data.frame(do.call("rbind", x))
}
