#' Get information about EUtils databases
#'
#' Gather information about EUtils generally, or a given Eutils database.
#'Note: The most common uses-cases for the einfo util are finding the list of
#' search fields available for a given database or the other NCBI databases to
#' which records in a given database might be linked. Both these use cases
#' are implemented in higher-level functions that return just this information
#' (\code{entrez_db_searchable} and \code{entrez_db_links} respectively).
#' Consequently most users will not have a reason to use this function (though
#' it is exported by \code{rentrez} for the sake of completeness.
#'@param db character database about which to retrieve information (optional)
#'@param config config vector passed on to \code{httr::GET}
#'@return XMLInternalDocument with information describing either all the
#'databases available in Eutils (if db is not set) or one particular database
#'(set by 'db')
#'@seealso \code{\link[httr]{config}} for available httr configurations
#'@family einfo
#'@importFrom XML xmlChildren xmlName xpathSApply
#'@examples
#'\dontrun{
#'all_the_data <- entrez_info()
#'XML::xpathSApply(all_the_data, "//DbName", xmlValue)
#'entrez_dbs()
#'}
#'@export

entrez_info <- function(db=NULL, config=NULL){
    req <- make_entrez_query("einfo", db=db, config=config)
    res <- parse_response(req, "xml")
    check_xml_errors(res)
    res
}

#' List databases available from the NCBI
#'
#' Retrieves the names of  databases available through the EUtils API
#'@param config config vector passed to \code{httr::GET}
#'@family einfo
#'@return character vector listing available dbs
#'@export
#'@examples
#'\donttest{
#' entrez_dbs()
#'}
entrez_dbs <- function(config=NULL){
    xpathSApply(entrez_info(config), "//DbName", xmlValue)
}


#' Retrieve summary information about an NCBI database
#'
#'@param config config vector passed to \code{httr::GET}
#'@param db character, name of database to summaries
#'@return Character vector with the following data
#'@return DbName Name of database
#'@return Description Brief description of the database
#'@return Count Number of records contained in the database
#'@return MenuName Name in web-interface to EUtils
#'@return DbBuild Unique ID for current build of database
#'@return LastUpdate Date of most recent update to database
#'@family einfo
#'@examples
#'entrez_db_summary("pubmed")
#'@export

entrez_db_summary <- function(db, config=NULL){
    rec <- entrez_info(db, config)
    unparsed <- xpathApply( rec, "//DbInfo/*[not(self::LinkList or self::FieldList)]")
    res <- sapply(unparsed, xmlValue)
    names(res) <- sapply(unparsed, xmlName)
    class(res) <- c("eInfoEntry", class(res))
    res
}


#' List available links for records from a given NCBI database
#'
#' For a given database, fetch a list of other databases that contain
#' cross-referenced records. The names of these records can be used as the
#' \code{db} argument in \code{\link{entrez_link}}
#'
#'@param config config vector passed to \code{httr::GET}
#'@param db character, name of database to search
#'@return An eInfoLink object (sub-classed from list) summarizing linked-databases.
#' Can be coerced to a data-frame with \code{as.data.frame}. Printing the object
#' the name of each element (which is the correct name for \code{entrez_link},
#' and can be used to get (a little) more information about each linked database
#' (see example below).
#'@family einfo
#'@seealso \code{\link{entrez_link}}
#'@examples
#' \donttest{
#'taxid <- entrez_search(db="taxonomy", term="Osmeriformes")$ids
#'tax_links <- entrez_db_links("taxonomy")
#'tax_links
#'entrez_link(dbfrom="taxonomy", db="pmc", id=taxid)
#'
#'sra_links <- entrez_db_links("sra")
#'as.data.frame(sra_links)
#'}
#'@export
entrez_db_links <- function(db, config=NULL){
    rec <- entrez_info(db, config)
    unparsed <- xpathApply(rec, "//Link", xmlChildren)
    res <- lapply(unparsed, lapply, xmlValue)
    res <- lapply(res, add_class, new_class='eInfoEntry')
    names(res) <- sapply(res, "[[", "DbTo")
    class(res) <- c("eInfoLink", "eInfoList", "list")
    attr(res, 'db') <- xmlValue(rec["/eInfoResult/DbInfo/DbName"][[1]])
    res
}


#' List available search fields for a given database
#'

#'Fetch a list of search fields that can be used with a given database. Fields
#' can be used as part of the \code{term} argument to \code{\link{entrez_search}}
#'@param config config vector passed to \code{httr::GET}
#'@param db character, name of database to get search field from
#'@return An eInfoSearch object (subclassed from list) summarizing linked-databases. 
#' Can be coerced to a data-frame with \code{as.data.frame}. Printing the object
#' shows only the names of each available search field. 
#'@seealso \code{\link{entrez_search}}
#'@family einfo
#'@examples
#'\donttest{
#' pmc_fields <- entrez_db_searchable("pmc")
#' pmc_fields[["AFFL"]]
#' entrez_search(db="pmc", term="Otago[AFFL]", retmax=0)
#' entrez_search(db="pmc", term="Auckland[AFFL]", retmax=0)
#'
#' sra_fields <- entrez_db_searchable("sra")
#' as.data.frame(sra_fields)
#'}
#'@export

entrez_db_searchable <- function(db, config=NULL){
    rec <- entrez_info(db, config)
    unparsed <- xpathApply(rec, 
                           "/eInfoResult/DbInfo/FieldList/Field",
                           xmlChildren)
    res <- lapply(unparsed, lapply, xmlValue)
    res <- lapply(res, add_class, new_class="eInfoEntry")
    names(res) <- sapply(res, "[[", "Name")
    class(res) <- c("eInfoSearch", "eInfoList", "list")
    attr(res, 'db') <- xmlValue(rec["/eInfoResult/DbInfo/DbName"][[1]])
    res
}

#'@export
print.eInfoLink<- function(x, ...){
        cat("Databases with linked records for database '", attr(x, "db"), "'\n", sep="")
        print(names(x), quote=FALSE)
}

#'@export
as.data.frame.eInfoList <- function(x, ...){
    data.frame(do.call("rbind", x), row.names=NULL)
}

#'@export
print.eInfoSearch <- function(x, ...){
    cat("Searchable fields for database '", attr(x, "db"), "'\n", sep="")
    for (term in x){
        cat(" ", term$Name, "\t", term$Description, "\n")  
    }
}

#'@export
print.eInfoEntry <- function(x, ...){
    cat(paste0(" ", names(x), ": ", unlist(x), collapse="\n"), "\n")
}
