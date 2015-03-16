#' Get links to datasets related to a unique ID from an NCBI database
#'
#' Constructs a query with the given arguments and downlands the XML
#' document created by that query. 
#'
#'@import XML
#'@export
#'@param db character Name of the database to search for links (or use "all" to 
#' search all databases available for \code{db}. \code{entrez_db_links} allows you
#' to discover databases that might have linked information (see examples).
#'@param dbfrom character Name of database from which the Id(s) originate
#'@param cmd link function to use
#'@param \dots character Additional terms to add to the request
#'@param config vector configuration options passed to httr::GET  
#'@seealso \code{\link[httr]{config}} for available configs 
#'@return An elink object containing vectors of unique IDs
#' the vectors names take the form [db_from]_[db_to]
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{xmlTreeParse}}
#' @examples
#' \donttest{
#'  (pubmed_search <- entrez_search(db = "pubmed", term ="10.1016/j.ympev.2010.07.013[doi]"))
#'  (linked_dbs <- entrez_db_links("pubmed"))
#'  nucleotide_data <- entrez_link(dbfrom = "pubmed", id = pubmed_search$ids, db ="nuccore")
#'  #All the links
#'  entrez_link(dbfrom="pubmed", db="all", id=pubmed_search$ids)
#'}
#'


entrez_link <- function(db, dbfrom, cmd='neighbor', config=NULL, ...){

    response <- make_entrez_query("elink", db=db, dbfrom=dbfrom, cmd=cmd,
                                  config=config, ..., 
                                  require_one_of=c("id", "WebEnv"))
    record <- parse_response(response, 'xml')
    search_ids <- XML::xpathSApply(record, "//IdList/Id", 
                                   function(x) as.numeric(XML::xmlValue(x))
    )
    Sys.sleep(0.33)
    parse_elink(record, cmd=cmd)
}

#    if(-1 %in% search_ids){
#       msg <- paste("Some  IDs not found in", dbfrom)
#       warning(msg)
#    }
#    db_names <- XML::xpathSApply(record, "//LinkName", XML::xmlValue)
#
#    # Get ID from each database
#    # Not simplified so if a single database get a named list (for consistancy)
#    result <- sapply(db_names, get_linked_ids, record=record, simplify=FALSE)
#    result$file <- record
#    class(result) <- c("elink", class(result))
#    #NCBI limits requests to three per second
#    Sys.sleep(0.33)
#    return(result)
#}


parse_elink <- function(x, cmd){
    check_xml_errors(x)
    res <- switch(cmd,
                  "neighbor"         = parse_neighbors(x),
                  "neighbor_score"   = parse_neighbors(x, scores=TRUE),
                  "neighbor_history" = parse_history(x),
                  "acheck"           = parse_acheck(x),
                  "ncheck"           = parse_check(x, "HasNeighbor"),
                  "lcheck"           = parse_check(x, "HasLinkOut"),
                  "llinkslib"        = parse_linkouts(x),
                  "llinks"           = parse_linkouts(x),
                  "prlinks"          = parse_linkouts(x),
                  parse_default(x, cmd)
    )
    class(res) <- c("elink", "list")
    res
}

parse_default <- function(x, cmd){
    warning(paste("Don't know how to deal with cmd", cmd, "returning xml file"))
    x
}

parse_neighbors <- function(x, scores=FALSE){
    content <- ""
    db_names <- XML::xpathSApply(x, "//LinkName", XML::xmlValue)
    links <- sapply(db_names, get_linked_elements, record=x, element="Id", simplify=FALSE)
    class(links) <- c("elink_classic", "list")
    if(scores){
        scores <- sapply(db_names, get_linked_elements, record=x, element="Score", simplify=FALSE)
        class(scores) <- c("elink_classic", "list")
        content <- " $scores: weighted neighbouring scores for each hit in links\n"
    }
    res <- list(links=links, scores=scores)
    attr(res, "content") <- paste(" $links: IDs for linked records from NCBI\n",
                                 content)
    res
}

parse_history <- function(x){
    qks <-    XML::xpathSApply(x, "//LinkSetDbHistory/QueryKey", XML::xmlValue, simplify=FALSE)
    names(qks) <-    XML::xpathSApply(x, "//LinkSetDbHistory/LinkName", XML::xmlValue)
    cookie <- XML::xmlValue(x[["//WebEnv"]])
    res <- list(WebEnv = cookie, QueryKeys=qks)
    attr(res, "content") <- paste0(" $WebEnv: A WebEnv (cookie) value \n",
                                   " $QueryKeys: A list of QUeryKeys for each included database")
    res
}

parse_acheck <- function(x){
    db_info <- XML::xpathApply(x, "//LinkInfo", XML::xmlToList)
    names(db_info) <-  sapply(db_info, "[[","LinkName")
    class(db_info)  <-  "elink_classic"
    res <- list(linked_databses = db_info)
    attr(res, "content") <- " $linked_databases: a list of summary data from each databse with linked records"
    res    
}

parse_check <- function(x, attr){
    path <- paste0("//Id/@", attr)
    is_it_y <- structure(names= XML::xpathSApply(x, "//Id", XML::xmlValue),
                     x[path] == "Y")
    res <- list(check = is_it_y)
    attr(res, "content") <- " $check: TRUE/FALSE for wether each ID has links"
    res
}

parse_linkouts <- function(x){
    per_id <- x["//IdUrlList/IdUrlSet"]
    list_per_id <- lapply(per_id, function(x) lapply(x["ObjUrl"], XML::xmlToList))
    names(list_per_id) <-paste0("ID_", sapply(per_id,function(x) XML::xmlValue(x[["Id"]])))
    list_o_lists <- lapply(list_per_id, unname)#otherwise first element of earch list has same name!
    list_o_lists <- lapply(list_o_lists, lapply, add_class, "linkout")
    res <- list( linkouts = list_o_lists)
    attr(res, "content") <- " $linkouts links to external websites"
    res
}


#' @export
print.elink <- function(x, ...){
    payload <- attr(x, "content")
    cat("elink object with contents\n", payload, "\n",sep="")
}


#' @export
print.linkout <- function(x,...){
    cat("Linkout from", x$Provider$Name, "\n $Url:", substr(x$Url, 1, 26), "...\n")
}
    
print.elink_classic <- function(x, ...){
   len <- length(x)
   cat(paste("elink result with information from", len - 1, "databases:\n"))
   print (names(x)[-len], quote=FALSE)
}
get_linked_elements <- function(record, dbname, element){
    path <-  paste0("//LinkSetDb/LinkName[text()='", dbname, "']/../Link/", element)
    return(XML::xpathSApply(record, path, XML::xmlValue))
}
