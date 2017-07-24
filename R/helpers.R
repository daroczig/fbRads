#' Return the name of the parent function
#' @return string
#' @keywords internal
#' @importFrom utils tail
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


#' Parse Facebook URL without bringing in another dependency. Might change this for urltools or httr
#' @param url string
#' @return list
#' @keywords internal
url_parse <- function(url) {

    schema <- sub('^(http[s]).*', '\\1', url)
    domain <- sub('^http[s]://([a-z0-9\\.-]*)/.*', '\\1', url)

    if (domain != 'graph.facebook.com') {
        stop('Not a Facebook API URL, to avoid unexpected behavior, use httr or urltools instead.')
    }

    version <- sub('^http[s]://[a-z0-9\\.-]*/v([0-9].[0-9]).*', '\\1', url)
    path    <- sub('^http[s]://[a-z0-9\\.-]*/v[0-9].[0-9]/(.*)\\?.*', '\\1', url)
    params  <- sapply(strsplit(sub('^.*\\?(.*)', '\\1', url), '&')[[1]], function(x) {
        x <- strsplit(x, split = '=')[[1]]
        setNames(URLdecode(x[2]), x[1])
    }, USE.NAMES = FALSE)

    list(
        schema  = schema,
        domain  = domain,
        version = version,
        path    = path,
        params  = as.list(params))

}
