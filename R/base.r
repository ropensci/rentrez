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
# the appropriate url. Required arguments for each endpoint are handled by
# specific funcitons. All of these functions can use the id_or_webenv() function
# (below) to ensure that at least on of these arguments are provided and the
# sleep_time() function to set the approrate time to wait between requests.
#
# if debug_mode is set to TRUE the function returns a list with the URL and 
# arguments that would have been passed to GET or POST (useful for debugging 
# and used in the test suite).

make_entrez_query <- function(util, config, interface=".fcgi?", by_id=FALSE, debug_mode=FALSE, ...){
    uri <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/", util, interface)
    args <- list(..., email=entrez_email(), tool=entrez_tool())
    if(!("api_key" %in% names(args))){ #no api key set, try to use the sytem var
        if(is_entrez_key_set()){
            args$api_key <- Sys.getenv('ENTREZ_KEY')
        }
    }
    if("id" %in% names(args)){
        if(by_id){
            ids_string <- paste0("id=", args$id, collapse="&")
            args$id <- NULL
            uri <- paste0(uri, ids_string)
        } else {
            args$id <- paste(args$id, collapse=",")
        }
    }
    if(debug_mode){
        return( list(uri = uri, args=args ) )
    }
    
    #Seemingly, NCBI moved to https but not http v2.0?
    # (thatnks Jeff Hammerbacher for report/solution)
    #
    # if no httr::config was passed we will add one
    if(is.null(config)){
         config = httr::config(http_version = 2)
    # otherwise add http version, checkign we aren't overwriting something
    # passed in (seems unlikely, but worth checking?)
    # 
    } else {
        if ("http_version" %in% names(config$options)) {
            warning("Over-writing httr config options for 'http_version', as NCBI servers require v1.1")
        }
        config$options$http_version <- 2        
    }

    
    if(length(args$id) > 200){ 
        response <- httr::POST(uri, body=args, config= config)
    } else {
        response <- httr::GET(uri, query=args, config= config) 
    }
    entrez_check(response)
    Sys.sleep(sleep_time(args))
    httr::content(response, as="text", encoding="UTF-8")
}


#set the sleep time, depending on presence of api_key in the arguments. Used by
# make_entrez_query. These add a little extra time as we still frequently hit
# the rate-limit when using 1/10 and 1/3 as times
sleep_time <- function(argument_list){
    if("api_key" %in% names(argument_list)){
        return(0.13)
    }
    0.35
}
##
# Check for that we have either the ID or the web-history functions are 
# specified for those functions that need one.
##

id_or_webenv <- function(){
    args <- sys.frame(sys.parent())
    msg <- "Must specify either (not both) 'id' or 'web_history' arguments" 
    if(!is.null(args$id)){
        if(!is.null(args$web_history)){
            stop(msg, call.=FALSE)
        }
        if (length(args$id) == 0){
            stop("Vector of IDs to send to NCBI is empty, perhaps entrez_search or entrez_link found no hits?", call.=FALSE)        
        }
        return(list(id=args$id))
    }
    if(is.null(args$web_history)){
        stop(msg, call.=FALSE)
    }
    list(WebEnv=args$web_history$WebEnv, query_key=args$web_history$QueryKey)
}


entrez_check  <- function(req){
  if (req$status_code < 400) {
      return(invisible())
  }
  if (req$status_code == 414){
      stop("HTTP failure 414, the request is too large. For large requests, try using web history as described in the rentrez tutorial")
  }
  if (req$status_code == 502){
      stop("HTTP failure: 502, bad gateway. This error code is often returned when trying to download many records in a single request.  Try using web history as described in the rentrez tutorial")
  }
  message <- httr::content(req, as="text", encoding="UTF-8")
  stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}


#Does  a parsed-xml object contains ERRORs as reported by NCBI
#(i.e. <ERROR> entry's in a valid XML):
check_xml_errors <- function(x){
    errs <- x["//ERROR"]
    if( length(errs) > 0){
        for(e in errs){
            warning(xmlValue(e))
        }
    }
    invisible()
}


parse_response <- function(x, type=NULL){
    res <- switch(type, 
            "json" = fromJSON(x),
            "xml"  = xmlTreeParse(x, useInternalNodes=TRUE),
            "native"  = xmlTreeParse(x, useInternalNodes=TRUE),
            "gbc"  = xmlTreeParse(x, useInternalNodes=TRUE),
            "ipg"  = xmlTreeParse(x, useInternalNodes=TRUE),
            "text" = x, #citmatch uses plain old plain text
             x #fall-through, if in doubt, return un-parsed response
    )
    return(res)
}

#contsructor for web history objects
web_history <- function(WebEnv, QueryKey){
    res <- list(WebEnv=WebEnv, QueryKey=QueryKey)
    class(res) <- list("web_history", "list")
    res
}

#'@export
print.web_history <- function(x, ...){
    cat("Web history object (QueryKey = ", x$QueryKey,
        ", WebEnv = ", substr(x$WebEnv, 1, 12), "...", ")\n",sep="")    
}



add_class <- function(x, new_class){
    class(x) <- c(new_class, class(x))
    x
}

.last <- function(s){
    len <- nchar(s)
    substr(s, len-1, len)
}
