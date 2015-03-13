#' rentrez
#'
#' rentrez provides functions to search for, discover and download data from
#' the NCBI's databases using their EUtils function. 
#'
#' Users are expected to know a little bit about the EUtils API, which is well
#' documented: \url{http://www.ncbi.nlm.nih.gov/books/NBK25500/}
#'
#' The NCBI will ban IPs that don't use EUtils within their \href{http://www.ncbi.nlm.nih.gov/corehtml/query/static/eutils_help.html}{user guidelines}. In particular
#' /enumerated{
#'  /item  Don't sen more than three request per second (rentrez enforces this limit)
#'  /item  If you plan on sending a sequence of more than ~100 requests, do so outside of peak times for the US
#'  /item  For large requests use the web history method (see examples for \code{\link{entrez_search}} or use \code{\link{entrez_post}} to upload IDs)
#'}
#' @docType package
#' @name rentrez
#' @aliases rentrez rentrez-package
#' 
NULL
