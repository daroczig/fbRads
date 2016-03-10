#' Return the name of the parent function
#' @return string
#' @keywords internal
this_function_name <- function() {
    sc <- sys.call(-1)[[1]]
    ifelse(is.symbol(sc),
           ## we have found the function name
           deparse(sc),
           ## this is not the actual function name but a call referring to that
    ifelse(sc[[1]] == '::',
           ## like a double colon
           deparse(tail(sc, 1)[[1]]),
           ## the calling function is returned by "get" or similar
           stop(paste("Something is off with the calling function's name:",
                      deparse(sc), "-- expecting a single function name here.",
                      'If in doubt, report this issue on <<bug.report()>>.'))
           ))
}
