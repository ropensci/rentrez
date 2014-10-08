#' Get summaries of objects in NCBI datasets from a unique ID 
#'
#' Contstructs a query from the given arguments, including a database name and
#' list of of unique IDs for that database.
#' 
#' The NCBI offer two distinct formats for summary docuements.
#' Version 1.0 is a relatively limited summary of a database record based on a 
#' shared Document Type Definition. Version 1.0 summaries are only avaliable as
#' XML and are not avaliable for some newer databases
#' Version 2.0 summaries generally contian more information about a given
#' record, but each database has its own distinct format. 2.0 summaries are 
#' avaliable for records in all databases and as JSON and XML files. 
#' As of version 0.4, rentrez fetches version 2.0 summaries by default and
#' uses JSON as teh exchange format (as JSON object can be more easily converted
#' into native R types). Existing scripts which relied on the stucture and
#' naming of the "Version 1.0" summary files updated through new \code{version}
#' argument.
#'
#'@export
#'@param db character Name of the database to search for
#'@param \dots character Additional terms to add to the request. Requires either
#'   id (unique id(s) for records in a given database) or WebEnv (a character
#'   containing a cookie created by a previous entrez query).
#'@param config vector configuration options passed to \code{httr::GET}
#'@param version either 1.0 or 2.0 see above for description
#'@seealso \code{\link[httr]{config}} for avaliable configs 
#'@return A list of esummary records (if multiple IDs are passed) or a single
#' record.
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{xmlTreeParse}}
#'@import XML
#' @examples
#'
#'  pop_ids = c("307082412", "307075396", "307075338", "307075274")
#'  pop_summ <- entrez_summary(db="popset", id=pop_ids)
#'  sapply(pop_summ, "[[", "Title")
#'  
#'  # clinvar example
#'  res <- entrez_search(db = "clinvar", term = "BRCA1")
#'  cv <- entrez_summary(db="clinvar", id=res$ids)
#'  cv[[1]] # get the names of the list for each result
#'  sapply(cv, "[[", "title") # titles
#'  lapply(cv, "[[", "trait_set")[1:2] # trait_set
#'  sapply(cv, "[[", "gene_sort") # gene_sort

entrez_summary <- function(db, version=c("2.0", "1.0"), config=NULL, ...){
    v <-match.arg(version)
    if(v == "2.0"){
        retmode <- "json"
    }else retmode <- "xml"
    response  <- make_entrez_query("esummary", db=db, config=config,
                                   retmode=retmode, version=v,
                                   require_one_of=c("id", "WebEnv"), ...)
    whole_record <- parse_respone(response, retmode)

    #Clinvar summary documents just have to be different...
    # special fxn for them defined below
    if(db == 'clinvar' & retmode == 'xml'){
        rec <- lapply(whole_record["//DocumentSummary"], parse_esummary_clinvar)
        if(length(rec)==1){
            rec <- rec[[1]]
        }
    }
    else {
      rec <- parse_esummary(whole_record)
    }
    return(rec)
}


parse_esummary <- function(x) UseMethod("parse_esummary")

reclass <- function(x){
    class(x) <- c("esummary", "list")
    x
}


parse_esummary.list <- function(x){
    #already parsed by jsonlite, just add class info to each one
    res <- x$result[2: length(x$result)]
    res <- lapply(res, reclass)
    if(length(res)==1){
        return(res[[1]])
    }
    return(res)
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
parse_esummary.XMLInternalDocument  <- function(x){
    recs <- x["//DocSum"]

    per_rec <- function(r){
        res <- xpathApply(r, "Item", parse_node)
        names(res) <- xpathApply(r, "Item", xmlGetAttr, "Name")
        res <- c(res, file=x)
        class(res) <- c("esummary", class(res))
        return(res)
    }
    if(length(recs) == 1){
        res <- per_rec(recs[[1]])
    } else{
        res <- lapply(recs, per_rec)
        names(res) <-  xpathSApply(x, "//DocSum/Id", xmlValue)
    }
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


parse_esummary_clinvar <- function(record){
  easynodes <- c('obj_type','accession','accession_version','title','supporting_submissions',
    'gene_sort','chr_sort','location_sort','variation_set_name')
  res <- sapply(easynodes, function(x) xpathApply(record, x, xmlValue))
  res$clinical_significance <- xpathApply(record, 'clinical_significance', xmlToList)[[1]]
  variation_set <- xpathApply(record, 'variation_set', xmlToList)[[1]]$variation
  variation_set$variation$aliases <- unlist(variation_set$variation$aliases, use.names = FALSE)
  trait_set <- xpathApply(record, 'trait_set', xmlToList)[[1]]$trait
  trait_set$trait$trait_xrefs <- unname(trait_set$trait$trait_xrefs)
  res$variation_set <- variation_set
  res$trait_set <- trait_set
  res <- c(res, file=record)
  class(res) <- c("esummary", class(res))
  return(res)
}

#' @export
`[[.esummary` <- function(x, name, ...){
    res <- NextMethod("[[") 
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




