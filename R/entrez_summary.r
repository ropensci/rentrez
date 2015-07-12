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
#'@export
#'@param db character Name of the database to search for
#'@param id vector with unique ID(s) for reacods in database \code{db}. 
#'@param web_history A web_history object 
#'@param always_return_list logical, return a list  of esummary objects even
#'when only one ID is provided.
#'@param \dots character Additional terms to add to the request. Requires either
#'   id (unique id(s) for records in a given database) or WebEnv (a character
#'   containing a cookie created by a previous entrez query).
#'@param config vector configuration options passed to \code{httr::GET}
#'@param version either 1.0 or 2.0 see above for description
#'@seealso \code{\link[httr]{config}} for available configs 
#'@seealso \code{\link{extract_from_esummarry}} which can be used to extract
#'elements from a list of esummary records
#'@return A list of esummary records (if multiple IDs are passed and
#'always_return_list if FALSE) or a single record.
#'@return file XMLInternalDocument xml file containing the entire record
#'returned by the NCBI.
#'@import XML
#' @examples
#'
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

entrez_summary <- function(db, id=NULL, web_history=NULL, 
                           version=c("2.0", "1.0"), always_return_list = FALSE, config=NULL, ...){
    identifiers <- id_or_webenv()
    v <-match.arg(version)
    retmode <- if(v == "2.0") "json" else "xml"
    args <- c(list("esummary", db=db, config=config, retmode=retmode, version=v, ...), identifiers)
    response  <- do.call(make_entrez_query, args)
    whole_record <- parse_response(response, retmode)
    parse_esummary(whole_record, always_return_list)
}

#' Extract elements from a list of esumarrry records
#'@export
#'@param esummaries A list of esummary objects
#'@param elements the names of the element to extract
#'@param simplify logical, if possible return a vector
#'@return List or vector containing requested elements 
extract_from_esummary <- function(esummaries, elements, simplify=TRUE){
    fxn <- if (simplify & length(elements) == 1) "[[" else "["
    sapply(esummaries, fxn, elements, simplify=simplify)
}




parse_esummary <- function(x, always_return_list) UseMethod("parse_esummary")


check_json_errs <- function(rec){
    if("error" %in% names(rec)){
        msg <- paste0("ID ", rec$uid, " produced error '", rec$error, "'")
        warning(msg, call.=FALSE)
    }
    invisible()
}


parse_esummary.list <- function(x, always_return_list){
    #already parsed by jsonlite, just add check for errors, then re-class
    res <- x$result[2: length(x$result)]
    sapply(res, check_json_errs)
    res <- lapply(res, add_class, new_class="esummary")
    if(length(res)==1 & !always_return_list){
        return(res[[1]])
    }
    class(res) <- c("esummary_list", "list")
    res
}

# Prase a sumamry XML 
#
# Logic goes like this
# 1. Define functions parse_esumm_* to handle all data types
# 2. For each node detect type, parse accordingly
# 3. wrap it all up in function parse_esummary that 
#

#
#@export
parse_esummary.XMLInternalDocument  <- function(x, always_return_list){
    check_xml_errors(x)
    recs <- x["//DocSum"]
    if(length(recs)==0){
       stop("Esummary document contains no DocSums, try 'version=2.0'?)")
    }
    per_rec <- function(r){
        res <- XML::xpathApply(r, "Item", parse_node)
        names(res) <- XML::xpathApply(r, "Item", XML::xmlGetAttr, "Name")
        res <- c(res, file=x)
        class(res) <- c("esummary", class(res))
        return(res)
    } 
    if(length(recs)==1 & !always_return_list){
        return(per_rec(recs[[1]]))
    } 
    res <- lapply(recs, per_rec)
    names(res) <-  XML::xpathSApply(x, "//DocSum/Id", XML::xmlValue)
    class(res) <- c("esummary_list", "list")
    res
}

parse_node <- function(node) {
    node_type <- XML::xmlGetAttr(node, "Type")

    node_fxn <- switch(node_type, 
                       "Integer" = parse_esumm_int,
                       "List" = parse_esumm_list,
                       "Structure" = parse_esumm_list,
                       XML::xmlValue) #unnamed arguments to switch = default val.
    return(node_fxn(node))

}

parse_esumm_int <- function(node) as.integer(XML::xmlValue(node))

parse_esumm_list <- function(node){
    res <- lapply(node["Item"], parse_node)
    names(res) <- lapply(node["Item"], XML::xmlGetAttr, "Name")
    return(res)
}



#' @export
`[[.esummary` <- function(x, name, ...){
    res <- NextMethod("[[") 
    if(is.null(res)){
        msg <- paste0("Esummary object '", deparse(substitute(x)), "' has no object",
                     " named '", name, "' \b.\nIf you were expecting values from a ",
                     " 'version 1.0' esummary record, try setting 'version' to ",
                     " '1.0' in entrez_summary (see documentation for more)\n")
        warning(msg)
     }
    res
}


#' @export
`[.esummary` <- function(x, ...){
    res <- NextMethod("[") 
    if(any(vapply(res, is.null, TRUE))){
        msg <- paste("Some values missing from Esumamry object. If you were",
                     "expecting a 'Version 1.0' esummary record, try setting",
                     "'version' to '1.0' in entrez_summary()")
       warning(msg) 
     }
    res
}



#' @export
`$.esummary` <- function(x, name){
    suppressWarnings(
        res <- x[[name]]
    )
    if(!is.null(res)){
        return(res)
    }
    suppressWarnings(res <- x[[name, exact=FALSE]])
    if(!is.null(res) &&  getOption("warnPartialMatchDollar", default=FALSE)){
        warning("Returning partial match")
        return(res)
    }
    if(is.null(res)){
         msg <- paste0("Esummary object '", deparse(substitute(x)), "' has no object",
                     " named '", name, "' \b.\nIf you were expecting values from a ",
                     " 'versoin 1.0' esummary record, try setting 'version' to ",
                     " '1.0' in entrez_summary (see documentation for more)\n")
        warning(msg)
    }
    res
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
    
