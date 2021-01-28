#' Get summaries of objects in NCBI datasets from a unique ID 
#
#' 
#' The NCBI offer two distinct formats for summary documents.
#' Version 1.0 is a relatively limited summary of a database record based on a 
#' shared Document Type Definition. Version 1.0 summaries are only available as
#' XML and are not available for some newer databases
#' Version 2.0 summaries generally contain more information about a given
#' record, but each database has its own distinct format. 2.0 summaries are 
#' available for records in all databases and as JSON and XML files. 
#' As of version 0.4, rentrez fetches version 2.0 summaries by default and
#' uses JSON as the exchange format (as JSON object can be more easily converted
#' into native R types). Existing scripts which relied on the structure and
#' naming of the "Version 1.0" summary files can be updated by setting the new
#' \code{version} argument to "1.0".
#'
#' By default, entrez_summary returns a single record when only one ID is
#' passed and a list of such records when multiple IDs are passed. This can lead
#' to unexpected behaviour when the results of a variable number of IDs (perhaps the
#' result of \code{entrez_search}) are processed with an apply family function
#' or in a for-loop. If you use this function as part of a function or script that
#' generates a variably-sized vector of IDs setting \code{always_return_list} to 
#' \code{TRUE} will avoid these problems. The function
#' \code{extract_from_esummary} is  provided for the specific case of extracting
#' named elements from a list of esummary objects, and is designed to work on
#' single objects as well as lists.
#'
#'@export
#'@param db character Name of the database to search for
#'@param id vector with unique ID(s) for records in database \code{db}.
#' In the case of sequence databases these IDs can take form of an
#' NCBI accession followed by a version number (eg AF123456.1 or AF123456.2)
#'@param web_history A web_history object 
#'@param always_return_list logical, return a list  of esummary objects even
#'when only one ID is provided (see description for a note about this option)
#'@param \dots character Additional terms to add to the request, see NCBI
#'documentation linked to in references for a complete list
#'@param config vector configuration options passed to \code{httr::GET}
#'@param version either 1.0 or 2.0 see above for description
#'@param retmode either "xml" or "json". By default, xml will be used for
#'version 1.0 records, json for version 2.0.
#'@references \url{https://www.ncbi.nlm.nih.gov/books/NBK25499/#_chapter4_ESummary_} 
#'@seealso \code{\link[httr]{config}} for available configs 
#'@seealso \code{\link{extract_from_esummary}} which can be used to extract
#'elements from a list of esummary records
#'@return A list of esummary records (if multiple IDs are passed and
#'always_return_list if FALSE) or a single record.
#'@return file XMLInternalDocument xml file containing the entire record
#'returned by the NCBI.
#'@importFrom XML xpathApply xmlSApply xmlGetAttr xmlValue
#'@importFrom jsonlite fromJSON
#' @examples
#'\dontrun{
#'  pop_ids = c("307082412", "307075396", "307075338", "307075274")
#'  pop_summ <- entrez_summary(db="popset", id=pop_ids)
#'  extract_from_esummary(pop_summ, "title")
#'  
#'  # clinvar example
#'  res <- entrez_search(db = "clinvar", term = "BRCA1", retmax=10)
#'  cv <- entrez_summary(db="clinvar", id=res$ids)
#'  cv
#'  extract_from_esummary(cv, "title", simplify=FALSE)
#'  extract_from_esummary(cv, "trait_set")[1:2] 
#'  extract_from_esummary(cv, "gene_sort") 
#' }
entrez_summary <- function(db, id=NULL, web_history=NULL, 
                           version=c("2.0", "1.0"), always_return_list = FALSE, retmode=NULL, config=NULL, ...){
    identifiers <- id_or_webenv()
    v <-match.arg(version) 
    if( is.null(retmode)) {
        retmode <- if( v == "1.0" ) "xml" else "json"
    }
    if (retmode == "json" & v == "1.0"){
        stop("Version 1.0 records are only available as xml, not json")
    }
    args <- c(list("esummary", db=db, config=config, retmode=retmode, version=v, ...), identifiers)
    response  <- do.call(make_entrez_query, args)
    whole_record <- parse_response(response, retmode)
    parse_esummary(whole_record, v, always_return_list)
}

#' Extract elements from a list of esummary records
#'@export
#'@param esummaries Either an esummary or an esummary_list (as returned by
#' entrez_summary).
#'@param elements the names of the element to extract
#'@param simplify logical, if possible return a vector
#'@seealso \code{\link{entrez_summary}} for examples of this function in action.
#'@return List or vector containing requested elements 
extract_from_esummary <- function(esummaries, elements, simplify=TRUE){
    UseMethod("extract_from_esummary", esummaries)
}

#'@export
extract_from_esummary.esummary <- function(esummaries, elements, simplify=TRUE){
    fxn <- if(simplify & length(elements)==1) `[[` else `[`
    fxn(esummaries, elements)
}

#'@export
extract_from_esummary.esummary_list <- function(esummaries, elements, simplify=TRUE){
    fxn <- if (simplify & length(elements) == 1) `[[` else `[`
    sapply(esummaries, fxn, elements, simplify=simplify)
}




parse_esummary <- function(x, version, always_return_list) UseMethod("parse_esummary")


check_json_errs <- function(rec){
    if("error" %in% names(rec)){
        msg <- paste0("ID ", rec$uid, " produced error '", rec$error, "'")
        warning(msg, call.=FALSE)
    }
    invisible()
}


parse_esummary.list <- function(x, version, always_return_list){
    #already parsed by jsonlite, just add check for errors, then re-class
    #First make sure the file doesn't have an error at the root
    if(!is.null(x[["error"]])){
        warning("Esummary includes error message: ", x[["error"]], call.=FALSE)
    }
    res <- x$result[-1] #remove UIDs from result (they are already names of sub-elements)    
    # Make sure there are some records in this file
    if(length(res) == 0){
        stop("No esummary records found in file", call.=FALSE)
    }
    #Finally check for errors _within_ each recods
    sapply(res, check_json_errs)    
    #OK: all clear, return the records
    res <- lapply(res, add_class, new_class="esummary")
    if(length(res)==1 & !always_return_list){
        return(res[[1]])
    }
    class(res) <- c("esummary_list", "list")
    res
}

# Parse a summary XML 
#
# Logic goes like this
# 1. Define functions parse_esumm_* to handle all data types
# 2. For each node detect type, parse accordingly
# 3. wrap it all up in function parse_summary that 
#

parse_esummary.XMLInternalDocument  <- function(x, version, always_return_list){
    check_xml_errors(x)
    #Version 2.0 records have no type information (int, list etc) so we 
    # can onyl return them as characters
    if(version == "2.0"){
        res <- lapply(x["//DocumentSummary"], xmlToList)
        res <- lapply(res, add_class, "esummary")
        names(res) <- sapply(res, function(x) x[[".attrs"]]["uid"])
    }
    else{
        recs <- x["//DocSum"] 

        if(length(recs)==0){
           stop("Esummary document contains no DocSums, try 'version=2.0'?)")
        }
        per_rec <- function(r){
            res <- xpathApply(r, "Item", parse_node)
            names(res) <- xpathApply(r, "Item", xmlGetAttr, "Name")
            res <- c(res, file=x)
            class(res) <- c("esummary", class(res))
            return(res)
        } 
        if(length(recs)==1 & !always_return_list){
            return(per_rec(recs[[1]]))
        } 
        res <- lapply(recs, per_rec)
        names(res) <-  xpathSApply(x, "//DocSum/Id", xmlValue)
    }
    class(res) <- c("esummary_list", "list")
    res
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


#' @export 
print.esummary <- function(x, ...){
    len <- length(x)
    cat(paste("esummary result with", len - 1, "items:\n"))
    print(names(x)[-len], quote=FALSE)
}

#' @export
print.esummary_list <- function(x, ...){
    len <- length(x)
    cat("List of ", len, "esummary records. First record:\n\n ")
    print(x[1])
}
    
