#' @S3method [[ xmlInternalDocument

"[[.xmlInternalDocument" <- function(xml, xpath){
    return( xpathSApply(xml, xpath, xmlValue ))
}
