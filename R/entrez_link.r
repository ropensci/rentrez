#' Get links to datasets related to records from an NCBI database
#'
#' Discover records related to a set of unique identifiers from
#' an NCBI database. The object returned by this function depends on the value
#' set for the \code{cmd} argument. Printing the returned object lists the names
#' , and provides a brief description, of the elements included in the object.
#'
#'@export
#'@param db character Name of the database to search for links (or use "all" to 
#' search all databases available for \code{db}. \code{entrez_db_links} allows you
#' to discover databases that might have linked information (see examples).
#'@param id vector with unique ID(s) for records in database \code{db}. 
#'@param web_history a web_history object  
#'@param dbfrom character Name of database from which the Id(s) originate
#'@param by_id logial If FALSE (default) return a single 
#' \code{elink} objects containing links for all of the provided \code{id}s. 
#' Alternatively, if TRUE return a list of \code{elink} objects, one for each 
#' ID in \code{id}. 
#'@param cmd link function to use. Allowled values include
#' \itemize{
#'   \item neighbor (default). Returns a set of IDs in \code{db} linked to the
#'   input IDs in \code{dbfrom}.
#'   \item neighbor_score. As `neighbor'', but additionally returns similarity scores.
#'   \item neighbor_history. As `neighbor', but returns web history objects.
#'   \item acheck. Returns a list of linked databases available from NCBI for a set of IDs.
#'   \item ncheck. Checks for the existence of links within a single database.
#'   \item lcheck. Checks for external (i.e. outside NCBI) links.
#'   \item llinks. Returns a list of external links for each ID, excluding links
#'   provided by libraries.
#'   \item llinkslib. As 'llinks' but additionally includes links provided by
#'   libraries.
#'   \item prlinks. As 'llinks' but returns only the primary external link for
#'   each ID.
#'}
#'@param \dots character Additional terms to add to the request, see NCBI
#'documentation linked to in references for a complete list
#'@param config vector configuration options passed to httr::GET  
#'@seealso \code{\link[httr]{config}} for available configs 
#'@seealso  \code{entrez_db_links}
#'@return An elink object containing the data defined by the \code{cmd} argument
#'(if by_id=FALSE) or a list of such object (if by_id=TRUE).
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{xmlTreeParse}}
#'@references \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#_chapter4_ELink_}
#'@importFrom XML xmlToList
#' @examples
#' \donttest{
#'  pubmed_search <- entrez_search(db = "pubmed", term ="10.1016/j.ympev.2010.07.013[doi]")
#'  linked_dbs <- entrez_db_links("pubmed")
#'  linked_dbs
#'  nucleotide_data <- entrez_link(dbfrom = "pubmed", id = pubmed_search$ids, db ="nuccore")
#'  #Sources for the full text of the paper 
#'  res <- entrez_link(dbfrom="pubmed", db="", cmd="llinks", id=pubmed_search$ids)
#'  linkout_urls(res)
#'}
#'


entrez_link <- function(dbfrom, web_history=NULL, id=NULL, db=NULL, cmd='neighbor', by_id=FALSE, config=NULL, ...){
    identifiers <- id_or_webenv()
    args <- c(list("elink", db=db, dbfrom=dbfrom, cmd=cmd, config=config, by_id=by_id, ...), identifiers)
    if(by_id){
        if(is.null(id)) stop("Can't use by_id mode without ids!")
    } 
    response <- do.call(make_entrez_query,args)
    record <- parse_response(response, 'xml')
    Sys.sleep(0.33)
    parse_elink(record, cmd=cmd, by_id=by_id)
}

#' Extract URLs from an elink object
#' @param elink elink object (returned by entrez_link) containing Urls
#' @return list of character vectors, one per ID each containing of URLs for that
#' ID.
#' @seealso entrez_link
#' @export
linkout_urls <- function(elink){
    if (!("linkouts" %in% names(elink))){
        stop("Not linkouts in the elink object. Use entrez_link commands 'prlinks', 'llinks' or 'llinkslib' to fetch urls")
    }
    lapply(elink$linkouts, function(lo) if(length(lo) == 0) NA else sapply(lo, "[[", "Url"))    
}


#
# Parising Elink is.... fun. The XML files returned by the different 'cmd'
# args are very differnt, so we can't hope for a one-size-fits all solution. 
# Instead, we can break of a few similar cases and write parsing functions, 
# which we dispatch via a big switch statement. 
#
# Each parsing function should return a list with elements corresponding to the
# data n XML, and set the attribute "content" to a brief description of what
# each element in the record contains, to be used by the print fxn.
#
# In addition, the "by_id" mode
# means we we sometimes reuturn a list of elink objects, have applied the
# relevant function to each "<LinkSet>" in the XML.
#
parse_elink <- function(x, cmd, by_id){
    check_xml_errors(x)
    f <- make_elink_fxn(cmd)
    res <-  xpathApply(x, "//LinkSet",f)
    if(length(res) > 1){
        class(res) <- c("elink_list", "list")
        return(res)
    }
    res[[1]]
}





make_elink_fxn <- function(cmd){
    f <- switch(cmd,
                  "neighbor"         = parse_neighbors,
                  "neighbor_score"   = function(x) parse_neighbors(x, scores=TRUE),
                  "neighbor_history" = parse_history,
                  "acheck"           = parse_acheck,
                  "ncheck"           = function(x) parse_check(x, "HasNeighbor"),
                  "lcheck"           = function(x) parse_check(x, "HasLinkOut"),
                  "llinkslib"        = parse_linkouts,
                  "llinks"           = parse_linkouts,
                  "prlinks"          = parse_linkouts,
                  stop("Don't know how to deal with cmd ", cmd)
    )
    function(x){
        res <- f(x)
        class(res) <- c("elink", "list")
        res
    }
    
}

parse_neighbors <- function(x, scores=FALSE){
    content <- ""
    if("-1" %in% xpathSApply(x, "//IdList/Id", xmlValue)){
       warning("Some IDs not found")
    }
    db_names <- xpathSApply(x, "LinkSetDb/LinkName", xmlValue)
    links <- sapply(db_names, get_linked_elements, record=x, element="Id", simplify=FALSE)
    class(links) <- c("elink_classic", "list")
    res <- list(links = links, file=x)
    if(scores){
        nscores <- sapply(db_names, get_linked_elements, record=x, element="Score", simplify=FALSE)
        class(nscores) <- c("elink_classic", "list")
        content <- " $scores: weighted neighbouring scores for each hit in links\n"
        res$scores <- nscores
    }
    attr(res, "content") <- paste(" $links: IDs for linked records from NCBI\n",
                                 content)
    res
}

parse_history <- function(x){
    qks <-    xpathSApply(x, "LinkSetDbHistory/QueryKey", xmlValue, simplify=FALSE)
    cookie <- xmlValue(x[["WebEnv"]])
    histories <- lapply(qks, web_history, WebEnv=cookie)
    names(histories) <-    xpathSApply(x, "//LinkSetDbHistory/LinkName", xmlValue)
    res <- list(web_histories=histories, file=x)
    attr(res, "content") <- paste0(" $web_histories: Objects containing web history information\n")
    res
}

parse_acheck <- function(x){
    db_info <- xpathApply(x, "//LinkInfo", xmlToList)
    names(db_info) <-  sapply(db_info, "[[","LinkName")
    class(db_info)  <-  "elink_classic"
    res <- list(linked_databses = db_info)
    attr(res, "content") <- " $linked_databases: a list of summary data from each databse with linked records"
    res    
}

parse_check <- function(x, attr){
    path <- paste0("IdCheckList/Id/@", attr)
    is_it_y <- structure(names= xpathSApply(x, "IdCheckList/Id", xmlValue),
                         xpathSApply(x, path, `==`, "Y"))
                   
    res <- list(check = is_it_y)
    attr(res, "content") <- " $check: TRUE/FALSE for wether each ID has links"
    res
}

parse_linkouts <- function(x){
    per_id <- xpathApply(x, "//IdUrlList/IdUrlSet")
    list_per_id <- lapply(per_id, function(x) lapply(x["ObjUrl"], xmlToList))
    names(list_per_id) <-paste0("ID_", sapply(per_id,function(x) xmlValue(x[["Id"]])))
    list_o_lists <- lapply(list_per_id, unname)#otherwise first element of earch list has same name!
    list_o_lists <- lapply(list_o_lists, lapply, add_class, "linkout")
    res <- list( linkouts = list_o_lists)
    attr(res, "content") <- " $linkouts: links to external websites"
    res
}




#' @export

print.elink_list <- function(x, ...){
    payload <- attr(x[[1]], "content")
    cat("List of", length(x), "elink objects,each containing\n", payload)
}

#' @export
print.elink <- function(x, ...){
    payload <- attr(x, "content")
    cat("elink object with contents:\n", payload, "\n",sep="")
}


#' @export
print.linkout <- function(x,...){
    cat("Linkout from", x$Provider$Name, "\n $Url:", substr(x$Url, 1, 26), "...\n")
}

#' @export
print.elink_classic <- function(x, ...){
   len <- length(x)
   cat(paste("elink result with information from", len , "databases:\n"))
   print (names(x), quote=FALSE)
}


get_linked_elements <- function(record, dbname, element){
    path <-  paste0("LinkSetDb/LinkName[text()='", dbname, "']/../Link/", element)
    return(xpathSApply(record, path, xmlValue))
}
