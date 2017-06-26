#' Safe version of fromJSON
#' @params txt JSON-encoded input
#' @importFrom jsonlite fromJSON
.safeFromJSON <- function(txt, ...) {
  txt <- gsub("\r", " ", txt)
  txt <- gsub("\n", " ", txt)
  txt <- gsub("\t", " ", txt)
  return(fromJSON(txt, ...))
}
