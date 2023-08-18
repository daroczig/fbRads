#' fbRads package
#'
#' This is a placeholder for storing import directives, please find more details in the \code{README.md} file of the package via \code{system.file} or on GitHub at \url{https://github.com/daroczig/fbRads}.
#' @docType package
#' @importFrom digest digest
#' @importFrom jsonlite toJSON fromJSON unbox validate
#' @importFrom RCurl postForm getForm getURL getURLContent getCurlHandle curlSetOpt basicHeaderGatherer basicTextGatherer curlOptions fileUpload
#' @importFrom logger log_info log_error log_debug log_trace log_threshold
#' @importFrom bit64 as.integer64
#' @importFrom plyr ldply
#' @name fbRads
NULL

.onLoad <- function(libname, pkgname) {
    logger::log_layout(logger::layout_simple, namespace = pkgname)
    logger::log_formatter(logger::formatter_sprintf, namespace = pkgname)
    logger::log_threshold(logger::TRACE, namespace = pkgname)
}
