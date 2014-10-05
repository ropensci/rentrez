#' Get infromation about EUtils databases
#'
#'Contstructs a query to NCBI's einfo and return parsed XML object
#'Note The most common uses for einfo API are to list fields against which
#'a database can be searched, or linked databases avaliabl,
#'@param db characater database about which to retreive information (optional,
#'@param config config vector passed on to \code{httr::GET}
#'@return XMLInternalDocument with information describing either all the
#'databases avaliable in Eutils (if db is not set) or one pariticular database
#'(set by 'db')
#'@seealso \code{\link[httr]{config}} for avaliable httr configurations
#'@family einfo
#'@examples
#'all_the_data <- entrez_info()
#'xpathSApply(all_the_data, "//DbName", xmlValue)
#'@export

entrez_info <- function(db=NULL, config=NULL){
    response <- make_entrez_query("einfo", db=db, config=config)
    return(xmlTreeParse(response, useInternalNodes=TRUE))
}

#' List databases avaliable from the NCBI
#'
#' Retreives a list of databases avaliable from NCBI
#'@param config config vector passedto \code{httr::GET}
#'@family einfo
#'@return character vector listing avaliable dbs
#'@export
#'@examples
#'\donttest{
#' entrez_dbs()
#'}
entrez_dbs <- function(config=NULL){
    xpathSApply(entrez_info(config), "//DbName", xmlValue)
}


#' Summarise a NCBI database
#'
#'@param config config vector passedto \code{httr::GET}
#'@param db character, name of database t
#'@return Character vector with the following data
#'@return DbName Name of database
#'@return Description Brief description of the database
#'@return Count Number of records contained in the databse
#'@return MenuName Name in web-interface to EUtils
#'@return DbBuild Unique ID for current build of databse
#'@return LastUpdate Date of most recent update to databse
#'@family einfo
#'@export

entrez_db_summary <- function(db, config=NULL){
    rec <- entrez_info(db, config)
    unparsed <- xpathApply( rec, "//DbInfo/*[not(self::LinkList or self::FieldList)]")
    res <- sapply(unparsed, xmlValue)
    names(res) <- sapply(unparsed, xmlName)
    res
}


# List avaliable links for records from a given database
#'
#'Can be used in conjunction with \code{\link{entrez_link}} to find
#' the right name for the \code{db_to} argument
#'@param config config vector passed to \code{httr::GET}
#'@param db character, name of database t
#'@return An eInfoLink object (subclassed from list) summarising linked-datbases.
#' Can be coerced to a data-frame with \code{as.data.frame}. Printing the object
#' the name of each element.
#'@family einfo
#'@seealso \code{\link{entrez_link}}
#'@export
entrez_db_links <- function(db, config=NULL){
    unparsed <- xpathApply(entrez_info(db, config), "//Link", xmlChildren)
    res <- lapply(unparsed, lapply, xmlValue)
    names(res) <- sapply(res, "[[", "Name")
    class(res) <- c("eInfoLink", "eInfoList", "list")
    return(res)
}


# List avaliable search fields for a given database
#
#' Can be used in conjunction with \code{\link{entrez_search}} to find avaliable
#' search fields to include in the \code{term} argument
#'@param config config vector passedto \code{httr::GET}
#'@param db character, name of database to get search field from
#'@return An eInfoSearch object (subclassed from list) summarising linked-datbases. 
#' Can be coerced to a data-frame with \code{as.data.frame}. Printing the object
#' shows only the names of each avaliable search field.
#'@seealso \code{\link{entrez_search}}
#'@family einfo
#'@export

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
