#What's going on under the hood. As far as possible we are following the best
#practices for API packages suggested by hadly/httr:
#
#  http://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
#
#and also conforming to the NBCI's requirements about rate limiting and 
#adding identifiers to each request:
#
# http://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.Usage_Guidelines_and_Requirements
#




#As per NCBI's documentation -- we set tool developer's email and tool name:
entrez_email <- function() 'david.winter@gmail.com'
entrez_tool <- function() 'rentrez'

#Create a URL for the EUtils API. 
#
# This function is used by all the API-querying functions in rentrez to build
# the appropriate url. Required arguments for each rentrez are handled in each
# function. Those arguments that either ID(s) or are WebEnv cookie can be set
# by passing a string or two argument names to `make_entrez_query`
#
#
# efetch_url <- make_entrez_query("efetch", require_one_of=c("id", "WebEnv"), 
#                                 id=c(23310964,23310965), db="pubmed",
#                                 rettype="xml")
#


make_entrez_query <- function(util, config, interface=".fcgi?", ...){
    args <- list(..., email=entrez_email(), tool=entrez_tool())
    if("id" %in% names(args)){
        args$id <- paste(args$id, collapse=",")      
    }
    uri <- paste0("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/", util, interface)
    response <- httr::GET(uri, query=args, config= config)
    entrez_check(response)
    return(httr::content(response, as="text"))
}

##
# Check for that we have either the ID or the web-history functions are 
# specified for those functions that need one.
##

id_or_webenv <- function(args){
    msg <- "Must specify either (not both) 'id' or web history arguments 'WebEnv' and 'query_key'"
    arg_names <- names(Filter(function(x) !is.null(x), args))
    cookie_args <- c("WebEnv", "query_key") %in% arg_names
    if("id" %in% arg_names){
        if(any(cookie_args)){
            stop(msg, call.=FALSE)
        }
        return(invisible())
    }
    if(!all(cookie_args)){
        stop(msg, call.=FALSE)
    }
    invisible()
}


entrez_check  <- function(req){
  if (req$status_code < 400) {
      return(invisible())
  }
  message <- httr::content(req)
  stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}



#Does  a parsed-xml object contains ERRORs as reported by NCBI
#(i.e. <ERROR> entry's in a valid XML):
check_xml_errors <- function(x){
    errs <- x["//ERROR"]
    if( length(errs) > 0){
        for(e in errs){
            warning(XML::xmlValue(e))
        }
    }
    invisible()
}


parse_response <- function(x, type=NULL){
    res <- switch(type, 
            "json" = jsonlite::fromJSON(x),
            "xml"  = XML::xmlTreeParse(x, useInternalNodes=TRUE),
            "text" = x, #citmatch uses plain old plain text
             x #fall-through, if in doubt, return un-parsed response
    )
    return(res)
}

add_class <- function(x, new_class){
    class(x) <- c(new_class, class(x))
    x
}

.last <- function(s){
    len <- nchar(s)
    substr(s, len-1, len)
}
