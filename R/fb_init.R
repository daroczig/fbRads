#' Returns the most recent version of the supported Facebook Marketing API
#' @return string
#' @export
fb_api_most_recent_version <- function() '3.0'

#' Returns the currently used version of the Facebook Marketing API
#' @return string
#' @export
fb_api_version <- function() {

    ## get the version of API currently used
    fbacc <- tryCatch(fbad_check_fbacc(),
                      ## or return the most recent version
                      error = function(e) list(api_version = fb_api_version))

    ## return
    fbacc$api_version

}


## initialize internal placeholder for FB Ad Account
fbacc <- list()


#' Run basic checks on curl get/post parameters
#' @param params named list of parameters to GET/POST
#' @return list if OK, error if not
#' @keywords internal
fbad_check_curl_params <- function(params) {

    ## Length check
    if (length(params) == 0) {
        stop("No parameters to get or set, pls debug this code")
    }

    ## Names check
    if (any(is.null(names(params)))) {
        stop("Parameters missing field names, expecting 'field = value'")
    }
    if (!"access_token" %in% names(params)) {
        stop("access_token missing")
    }

    ## Values check (not required)
    if (any(as.vector(unlist(params)) %in% c(NA, NULL))) {
        stop("NA or NULL values not allowed in cURL request")
    }

    ## Return params if all OK
    params

}


#' Get results of a synchronous query from FB graph API
#' @param fbacc (optional) \code{FB_Ad_account} object, which defaults to the last returned object of \code{\link{fbad_init}}.
#' @param path API request path (i.e. endpoint)
#' @param method HTTP request type (e.g. GET or POST or DELETE)
#' @param params a name-value list of form parameters for API query
#' @param debug print debug messages by calling Curl verbosely
#' @param log print log messages or suppress those
#' @param version Facebook Marketing API version, defaults to what was used in the most recent \code{fbad_init} function call or \code{fb_api_most_recent_version()} before such function call
#' @param retries number of times the current query was tried previously -- used to handle network errors
#' @return json object containing results
#' @keywords internal
#' @importFrom utils getFromNamespace URLencode capture.output str
fbad_request <- function(fbacc, path, method = c('GET', 'POST', 'DELETE'), params = list(), debug = FALSE, log = TRUE, version, retries = 0) {

    mc     <- match.call()
    method <- match.arg(method)

    ## if token was not set in params, try to do that from fbacc
    if (is.null(params$access_token)) {
        if (missing(fbacc)) {
            params$access_token <- getFromNamespace('fbacc', 'fbRads')$access_token
        } else {
            params$access_token <- fbad_check_fbacc(fbacc)$access_token
        }
    }

    ## define Facebook API version to be used
    if (missing(version)) {
        if (missing(fbacc)) {
            if (is.FB_Ad_Account(getFromNamespace('fbacc', 'fbRads'))) {
                version <- getFromNamespace('fbacc', 'fbRads')$api_version
            } else {
                version <- fb_api_most_recent_version()
            }
        } else {
            version <- fbad_check_fbacc(fbacc)$api_version
        }
    }

    ## check that params meet certain standards
    params <- fbad_check_curl_params(params)

    ## get body
    b = basicTextGatherer(.mapUnicode = FALSE)

    ## get headers
    h = basicHeaderGatherer()

    ## debug
    if (debug) {
        print(params)
    }

    ## remove prefix from the path if already provided
    path <- sub(paste0('^https://graph.facebook.com/v', version, '/'), '', path)

    ## versioned API endpoint
    API_endpoint <- paste(
        'https://graph.facebook.com',
        paste0('v', version),
        path, sep = '/')

    ## query
    if (method == 'DELETE') {

        curlres <- tryCatch(res <- getURLContent(
                                url   = paste0(API_endpoint, '?',
                                               paste(mapply(function(id, v)
                                                            paste(URLencode(id),
                                                                  URLencode(v),
                                                                  sep = '='),
                                                            names(params),
                                                            params),
                                                     collapse = '&')),
                                .opts = curlOptions(
                                    headerfunction = h$update,
                                    verbose   = debug,
                                    writefunc = b$update,
                                    customrequest = 'DELETE',
                                    cainfo    = system.file(
                                        'CurlSSL',
                                        'cacert.pem',
                                        package = 'RCurl'),
                                    connecttimeout = 3,
                                    timeout = 300)),
                            error = function(e) e)

    } else {

        curlres <- tryCatch(res <- do.call(what = paste0(
                                               ifelse(method == 'GET', 'get', 'post'),
                                               'Form'),
                                           args = list(
                                               uri     = API_endpoint,
                                               .params = params,
                                               .opts = curlOptions(
                                                   headerfunction = h$update,
                                                   verbose   = debug,
                                                   writefunc = b$update,
                                                   cainfo    = system.file(
                                                       'CurlSSL',
                                                       'cacert.pem',
                                                       package = 'RCurl'),
                                                   crlf = ifelse(method == 'GET',
                                                                 TRUE, FALSE),
                                                   connecttimeout = 3,
                                                   timeout = 300))),
                            error = function(e) e)
    }

    ## remove token from params if printed for debugging purposes
    params$token <- params$access_token <- NULL

    ## Curl error handling
    if (inherits(curlres, 'error')) {

        ## temporary network issue?
        if (grepl('Network is unreachable', curlres$message) |
            grepl('Empty reply from server', curlres$message) |
            grepl('Failed to connect to graph.facebook.com', curlres$message) |
            grepl('(Connection|Operation) timed out after', curlres$message) |
            grepl('Unknown SSL protocol error', curlres$message) |
            grepl('OpenSSL SSL_connect: SSL_ERROR_SYSCALL in connection to graph.facebook.com', curlres$message) |
            grepl('OpenSSL SSL_read: SSL_ERROR_SYSCALL', curlres$message)) {

            ## log it
            flog.error(paste('Possible network error:', curlres$message), name = 'fbRads')
            flog.info(paste('Retrying query for the', retries + 1, ' st/nd/rd time'), name = 'fbRads')

            ## give some chance for the system/network to recover
            Sys.sleep(2)

            ## retry the query for no more than 3 times
            if (retries < 3) {
                mc$retries <- retries + 1
                return(eval(mc, envir = parent.frame()))
            }

        }

        res <- curlres

    }

    ## Response error handling
    if (inherits(res, 'error')) {

         if (log) {
            flog.error(paste('URL: ', API_endpoint), name = 'fbRads')
            flog.error(paste('Method: ', method), name = 'fbRads')
            flog.error(paste('Params: ', paste(capture.output(str(params)), collapse = '\n')), name = 'fbRads')
        }

        stop(paste(
            ifelse(inherits(curlres, 'error'),
                   'This is a bug in the fbRads package. Please report on GitHub with a detailed output:',
                   'FB query failed:'),
            res$message))

    }

    ## Capture return value
    res     <- b$value()
    headers <- as.list(h$value())

    ## Response code error handling
    if (headers$status != '200') {

        ## log details of the error
        if (log) {
            flog.error(paste('URL: ', API_endpoint), name = 'fbRads')
            flog.error(paste('Method: ', method), name = 'fbRads')
            flog.error(paste('Params: ', paste(capture.output(str(params)), collapse = '\n')), name = 'fbRads')
            flog.error(paste('Header:', toJSON(headers)), name = 'fbRads')
            flog.error(paste('Body:', res), name = 'fbRads')
        }

        ## retry if Service (temporarily) Unavailable
        if (headers$status %in% c('502', '503', '504')) {

            ## give some chance for the system/network to recover
            Sys.sleep(2)

            ## retry the query for no more than 3 times
            if (retries < 3) {
                flog.info(paste('Retrying query for the', retries + 1, ' st/nd/rd time'), name = 'fbRads')
                mc$retries <- retries + 1
                return(eval(mc, envir = parent.frame()))
            }

        }

        ## something nasty happened that we cannot help (yet)
        if (inherits(tryCatch(fromJSONish(res), error = function(e) e), 'error') ||
            is.null(fromJSONish(res))) {
            stop('Some critical FB query error here.')
        }

        ## otherwise it's a JSON response
        res <- fromJSONish(res)

        ## temporary "API Unknown" (1) or "API Service" (2) error at FB
        if (res$error$code %in% 1:2) {

            ## log it
            flog.error(paste('This is a temporary',
                             shQuote(res$error$type),
                             'FB error:',
                             res$error$message),
                       name = 'fbRads')

            ## give some chance for the system/network to recover
            Sys.sleep(2)

            ## retry the query for no more than 3 times
            if (retries < 3) {
                flog.info(paste('Retrying query for the', retries + 1, ' st/nd/rd time'), name = 'fbRads')
                mc$retries <- retries + 1
                return(eval(mc, envir = parent.frame()))
            }

        }

        ## fail with (hopefully) meaningful error message
        stop(res$error$message)

    }

    ## return
    res

}


#' Get details for a Facebook Ads Account
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-account}
#' @param accountid Ads account graph object id
#' @param token FB Ads API token
#' @param version Facebook Marketing API version
#' @return list(s) containing account details
#' @export
fbad_get_adaccount_details  <- function(accountid, token, version) {

    ## Check if accountid is a vector
    if (length(accountid) > 1)
        lapply(accountid, fbad_get_adaccount_details)

    ## Define which fields to get
    scope <- paste(
        c('name', 'account_id', 'account_status',
          'age', 'amount_spent', ##'balance', ## 'capabilities',
          'end_advertiser', 'funding_source',
          'spend_cap', 'timezone_id', 'users'),
        collapse = ',')

    ## Get account details
    account_details <-
        fbad_request(
            path   = paste0('act_', accountid, '/'),
            method = 'GET',
            params = list(
                access_token = token,
                fields       = scope),
            version = version)

    fromJSONish(account_details)

}


#' Fetch the content of the URL and return JSON
#' @param url next URL as returned by FB
#' @keywords internal
fbad_request_next_page <- function(url) {
    url <- url_parse(url)
    res <- fbad_request(path = url$path, method = 'GET', params = url$params, version = url$version)
    fromJSONish(res)
}

#' Get account details of Ad Accounts owned by a Business Manager Account
#' @references \url{https://developers.facebook.com/docs/marketing-api/businessmanager/assets#adaccounts}
#' @param id Facebook Object, eg Ad Account (with \code{act} prefix) or a Business Manager Account ID
#' @param token FB Ads API token (if running before \code{fb_init})
#' @param version Facebook Marketing API version (if running before \code{fb_init})
#' @param fields character vector
#' @param simplify return \code{data.frame} or \code{list}
#' @return list(s) containing account details
#' @export
fbad_get_owned_ad_accounts <- function(id, token, version, fields = c('name'), simplify = TRUE) {

    ## look up function name to know what API endpoint to use
    fn <- deparse(match.call()[[1]])

    ## try to look up token and version if not provided
    if (missing(token)) {
        if (is.null(fbacc$access_token)) {
            stop('Missing Facebook Ads API token')
        }
        token <- fbacc$access_token
    }
    if (missing(version)) {
        if (is.null(fbacc$api_version)) {
            version <- fb_api_most_recent_version()
        } else {
            version <- fbacc$api_version
        }
    }

    res <- fromJSONish(fbad_request(
        path   = file.path(id, switch(fn,
                                      'fbad_get_client_ad_accounts' = 'client_ad_accounts',
                                      'fbad_get_owned_ad_accounts' = 'owned_ad_accounts',
                                      'fbad_get_adaccounts' = 'owned_ad_accounts',
                                      'fbad_get_client_pages' = 'client_pages',
                                      'fbad_get_owned_pages' = 'owned_pages',
                                      'fbad_get_pixels' = 'adspixels')),
        method = 'GET',
        params = list(
            access_token = token,
            fields       = fields),
        version = version))
    data <- list(res$data)

    ## iterate through all pages
    while (!is.null(res$paging$`next`)) {
        res  <- fbad_request_next_page(res$paging$`next`)
        data <- c(data, list(res$data))
    }

    if (simplify) {
        data <- do.call(rbind, data)
    }

    ## return
    data

}


#' Get account details of Ad Accounts belonging to the clients of a Business Manager Account
#' @inheritParams fbad_get_owned_ad_accounts
#' @export
fbad_get_client_ad_accounts <- fbad_get_owned_ad_accounts


#' Deprecated in favor of \code{fbad_get_owned_adaccounts}
#' @inheritParams fbad_get_owned_ad_accounts
#' @export
fbad_get_adaccounts <- fbad_get_owned_ad_accounts


#' Get account details of Pages belonging to the clients of a Business Manager Account
#' @inheritParams fbad_get_owned_ad_accounts
#' @export
fbad_get_client_pages <- fbad_get_owned_ad_accounts


#' Get account details of Pages owned by a Business Manager Account
#' @inheritParams fbad_get_owned_ad_accounts
#' @export
fbad_get_owned_pages <- fbad_get_owned_ad_accounts


#' Get tracking pixels of eg an Ad or Business Manager Account
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ads-pixel/#Reading}
#' @inheritParams fbad_get_owned_ad_accounts
#' @param token FB Ads API token
#' @param version Facebook Marketing API version
#' @param fields character vector
#' @return list(s) containing Ads Pixels
#' @export
fbad_get_pixels <- fbad_get_owned_ad_accounts


#' Get account details of Ad Accounts that are accessible by the given token
#' @inheritParams fbad_get_owned_ad_accounts
#' @export
#' @return character vector of Ad Account ids
fbad_get_my_ad_accounts <- function(token, version) {

    ## try to look up token and version if not provided
    if (missing(token)) {
        if (is.null(fbacc$access_token)) {
            stop('Missing Facebook Ads API token')
        }
        token <- fbacc$access_token
    }
    if (missing(version)) {
        if (is.null(fbacc$api_version)) {
            version <- fb_api_most_recent_version()
        } else {
            version <- fbacc$api_version
        }
    }

    res <- fromJSONish(fbad_request(
        path   = 'me',
        method = 'GET',
        params = list(access_token = token, fields = 'adaccounts'),
        version = version))[[1]]
    data <- list(res$data)

    ## iterate through all pages
    while (!is.null(res$paging$`next`)) {
        res  <- fbad_request_next_page(res$paging$`next`)
        data <- c(data, list(res$data))
    }

    ## return
    do.call(rbind, data)

}


#' Initiate Facebook Account with OAuth token
#'
#' If you do not have a token, then register an (e.g. "Website") application at \url{https://developers.facebook.com/apps} and make a note of your "App ID" and "App Secret" at the "Dashboard" of your application. Then go to "Settings", click on "Add Platform", then "Website" and paste \code{http://localhost:1410} as the "Site URL". Save, and then run the below example R commands to get your token. Please note that your app needs access to your ads as well, see \url{https://developers.facebook.com/docs/marketing-api/access} for more details.
#' @param accountid Facebook Ad account id without the \code{act_} prefix
#' @param token Facebook OAuth token as a string
#' @param version Facebook Marketing API version
#' @return list returned invisibly containing versioned base URL and relevant API parameters
#' @export
#' @examples \dontrun{
#' ## You can generate a token for future use with the help of `httr`, e.g.
#' library(httr)
#' app <- oauth_app("facebook", your_app_id,  your_app_secret)
#' oauth2.0_token(oauth_endpoints("facebook"), app,
#'   scope = '',
#'   type  = "application/x-www-form-urlencoded")$credentials$access_token
#'
#' ## Then pass this token with your account ID to fbad_init
#' }
#' @importFrom utils assignInMyNamespace
fbad_init <- function(accountid, token, version = fb_api_most_recent_version()) {

    flog.trace(paste0('Initializing connection to account ', accountid,
                      ' via API v', version),
               name = 'fbRads')

    ## API endpoint
    url <- paste(
        'https://graph.facebook.com',
        paste0('v', version),
        '', sep = '/')

    ## define parameters
    params <- list(
        acct_path     = paste0('act_', accountid, '/'),
        versioned_url = url,
        access_token  = token,
        api_version   = version)

    ## get account details
    details <- fbad_get_adaccount_details(accountid, token, version)

    ## add custom class
    res <- structure(c(params, details), class = 'FB_Ad_Account')

    ## save FB Ad Account in the internal namespace
    ## so that we can reuse that later without directly referencing it
    assignInMyNamespace('fbacc', res)

    ## someone might want to use this object directly
    invisible(res)

}

#' Check if provided or previously initialized  R object is a valid FB Ad Account
#' @inheritParams fbad_request
#' @return invisibly returned \code{FB_Ad_Account} object
#' @keywords internal
fbad_check_fbacc <- function(fbacc) {

    ## no fbacc argument was provided
    if (missing(fbacc)) {

        ## so let's match the parent call for it (if available)
        if (length(sys.calls()) > 1) {
            mc <- match.call(definition = sys.function(-1), call = sys.call(-1))
        } else {
            mc <- list()
        }

        ## manually passed fbacc object found in parent call
        if (!is.null(mc$fbacc)) {

            fbacc <- eval.parent(mc$fbacc)

        } else {

            ## or get it from the pkg namespace (default) if available
            fbacc <- getFromNamespace('fbacc', 'fbRads')

            if (is.null(fbacc)) {

                ## otherwise we are in trouble
                stop('No FB Ad Account object previously specified or directly passed to check. See ?fbad_init for more details.')

            }

        }
    }

    ## verify object type
    if (!is.FB_Ad_Account(fbacc)) {
        stop('Invalid R object passed as fbacc argument. See ?fbad_init for more details.')
    }

    ## return object from any source
    invisible(fbacc)

}


#' Print method for custom fbRads class
#' @param x R object with \code{FB_Ad_Account} class
#' @param ... further arguments passed to \code{print} (ignored)
#' @export
print.FB_Ad_Account <- function(x, ...) {
    cat(paste0('Facebook Ad API connection parameters for <<', x$name, '>>.'), '\n')
}


#' Checks if object is a valid initialized connection to the Facebook Marketing API
#' @param fbacc R object with \code{FB_Ad_Account} class
#' @keywords internal
is.FB_Ad_Account <- function(fbacc) {
    inherits(fbacc, 'FB_Ad_Account')
}
