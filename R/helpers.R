#' Return the name of the parent function
#' @return string
#' @keywords internal
this_function_name <- function() {
    sc <- sys.call(-1)[[1]]
    ifelse(is.symbol(sc),
           ## we have found the function name
           deparse(sc),
           ## this is not the actual function name but a call referring to that
           deparse(tail(sc, 1)[[1]]))
}
