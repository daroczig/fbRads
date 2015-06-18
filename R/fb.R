## we build the functions to support one given version of the API
## so the users should not be able to override that
fbad_api_version <- 2.3


#' Get versioned base url
#' @param version the version for which a base url is being generated
#' @return character URL with trailing slash
#' @keywords internal
fbad_get_baseurl <- function() {
  paste(
      'https://graph.facebook.com',
      paste0('v', fbad_api_version),
      '', sep = '/')

}


#' Run basic checks on curl get/post parameters
#' @param params named list of parameters to GET/POST
#' @return list if OK, error if not
#' @keywords internal
fbad_check_curl_params <- function(params){

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
#' @param path API request path (i.e. endpoint)
#' @param method HTTP request type (e.g. GET or POST or DELETE)
#' @param params a name-value list of form parameters for API query
#' @param debug print debug messages by calling Curl verbosely
#' @return json object containing results
#' @keywords internal
fbad_request <- function(path, method = c('GET', 'POST', 'DELETE'), params, debug = FALSE) {

    method <- match.arg(method)

    ## check that params meet certain standards
    params <- fbad_check_curl_params(params)

    ## get body
    b = basicTextGatherer()

    ## get headers
    h = basicHeaderGatherer()

    ## debug
    if (debug) {
        print(params)
    }

    ## query
    curlres <- tryCatch(res <- do.call(what = paste0(
                                           ifelse(method == 'GET', 'get', 'post'),
                                           'Form'),
                                       args = list(
                                           uri     = paste0(fbad_get_baseurl(), path),
                                           .params = params,
                                           .opts = curlOptions(
                                               headerfunction = h$update,
                                               verbose = debug,
                                               writefunc = b$update))),
                        error = function(e) e)

    ## remove token from params if printed for debugging purposes
    params$token <- params$access_token <- NULL

    ## Curl error handling
    if (inherits(curlres, 'error')) {
        res <- curlres
    }

    ## Response error handling
    if (inherits(res, 'error')) {
        flog.error(paste('URL: ', paste0(fbad_get_baseurl(), path)))
        flog.error(paste('Method: ', method))
        flog.error(paste('Params: ', paste(capture.output(str(params)), collapse = '\n')))
        stop(paste(
            ifelse(inherits(curlres, 'error'),
                   'This is a bug in the fbRads package. Please report on GitHub:'
                   'FB query failed:'),
            res$message))
    }

    ## return value
    res     <- b$value()
    headers <- as.list(h$value())

    ## Response code error handling
    if (headers$status != '200') {
        flog.error(paste('URL: ', paste0(fbad_get_baseurl(), path)))
        flog.error(paste('Method: ', method))
        flog.error(paste('Params: ', paste(capture.output(str(params)), collapse = '\n')))
        flog.error(paste('Header:', toJSON(headers)))
        flog.error(paste('Body:', res))
        if (!inherits(tryCatch(fromJSON(res), error = function(e) e), 'error') &&
            !is.null(fromJSON(res))) {
            stop(fromJSON(res)$error$message)
        } else {
            stop('Some critical FB query error here.')
        }
    }

    ## return
    res

}


#' Get details for a Facebook Ads Account
#' @references https://developers.facebook.com/docs/marketing-api/adaccount/v2.2
#' @param accountid Ads account graph object id
#' @param token FB Ads API token
#' @return list(s) containing account details
#' @export
fbad_get_adaccount_details  <- function(accountid, token){

    ## Check if accountid is a vector
    if (length(accountid) > 1)
        lapply(accountid, fbad_get_adaccount_details)

    ## Define which fields to get
    scope <- paste(
        c('name', 'account_id', 'account_status',
          'age', 'amount_spent', 'balance', 'capabilities',
          'daily_spend_limit', 'end_advertiser', 'funding_source',
          'spend_cap', 'timezone_id', 'users'),
        collapse = ',')

    ## Get account details
    account_details <-
        fbad_request(
            path   = paste0('act_', accountid, '/'),
            method = 'GET',
            params = list(
                access_token = token,
                fields       = scope)
            )

    fromJSON(account_details)

}


#' Initiate Facebook Account with OAuth token
#'
#' If you do not have a token, then register an (e.g. "Website") application at \url{https://developers.facebook.com/apps} and make a note of your "App ID" and "App Secret" at the "Dashboard" of your application. Then go to "Settings", click on "Add Platform", then "Website" and paste \code{http://localhost:1410} as the "Site URL". Save, and then run the below example R commands to get your token. Please note that your app needs access to your ads as well, see \url{https://developers.facebook.com/docs/marketing-api/access} for more details.
#' @param accountid Facebook Ad account id without the \code{act_} prefix
#' @param token Facebook OAuth token as a string
#' @return list containing versioned base URL and relevant API parameters
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
fbad_init <- function(accountid, token) {

    ## define parameters
    params <- list(
        acct_path     = paste0('act_', accountid, '/'),
        versioned_url = fbad_get_baseurl(),
        access_token  = token,
        api_version   = fbad_api_version
        )

    ## get account details
    details <- fbad_get_adaccount_details(accountid, token)

    ## add custom class and return
    structure(c(params, details), class = 'FB_Ad_Account')

}

#' Check if provided R object is a valid list of FB account details
#' @param fbacc R object
#' @keywords intenral
fbad_check_fbacc <- function(fbacc) {

    if (missing(fbacc))
        stop('Please initialize and pass your FB Ad account object. See ?fbad_init for more details.')
    if (!inherits(fbacc, 'FB_Ad_Account'))
        stop('Invalid R object passed as fbacc argument. See ?fbad_init for more details.')

}


#' Print method for custom fbRads class
#' @param x R object with \code{FB_Ad_Account} class
#' @param ... further arguments passed to \code{print} (ignored)
#' @export
print.FB_Ad_Account <- function(x, ...) {
    cat(paste0('Facebook Ad API connection parameters for <<', x$name, '>>.'), '\n')
}


#' FB Search API Querying
#' @references https://developers.facebook.com/docs/marketing-api/targeting-search/v2.2
#' @param fbacc FB_Ad_account object returned by \code{fbad_init}
#' @param q string that is being searched for
#' @param type describes the type of search eg: adinterest, adeducationmajor etc
#' @param ... other optional parameters accepted by the endpoint as key = value pairs eg: \code{limit = 5000}.
#' @return \code{data.frame} containing results
#' @examples \dontrun{
#' fbacc <- fbad_init(...)
#' fbad_get_search(fbacc, c('dog', 'cat'), type = 'adinterestvalid')
#' }
#' @export
fbad_get_search <- function(
    fbacc, q,
    type = c(
        'adeducationschool', 'adeducationmajor',
        'adgeolocation', 'adcountry', 'adregion', 'adcity', 'adzipcode', 'adgeolocationmeta', 'adradiussuggestion',
        'adinterest', 'adinterestsuggestion', 'adinterestvalid',
        'adlocale', 'adTargetingCategory', 'adworkemployer'), ... ) {

    type <- match.arg(type)
    fbad_check_fbacc(fbacc)

    ## default params
    params <- list(access_token = fbacc$access_token,
                   limit        = 500,
                   type         = type,
                   list         = "GLOBAL")

    ## update params
    if (length(list(...)) > 0) {
        params <- c(defaults, list(...))
    }

    ## Handle term input variation in API
    if (type %in% c("adinterestvalid",
                         "adinterestsuggestion")) {

        params <- c(params, list(interest_list = toJSON((q))))

    } else {

        if (length(q) > 1) {
            warning("Multiple keywords not allowed. Using first")
            q <- q[1]
        }

        params <- c(params, list("q" = as.character(q)))

    }

    ## get results
    properties <- fbad_request(
        path   = "search",
        method = "GET",
        params = params)

    ## transform data into data frame
    res <- fromJSON(properties)$data

    ## list to data.frame with know colnames
    if (type %in% c(
        "adinterestvalid",
        "adinterestsuggestion",
        "adinterest")) {

        if (inherits(res, 'list')) {
            res  <- sapply(res, function(x) c(
                id            = x$id,
                name          = x$name,
                audience_size = x$audience_size))
        }
    }

    if (!inherits(res, 'data.frame')) {
        res <- ldply(res, as.data.frame)
    }

    ## return
    res

}


#' Create a new FB custom audience
#' @references https://developers.facebook.com/docs/marketing-api/custom-audience-targeting/v2.2#create
#' @param fbacc FB_Ad_account object returned by \code{fbad_init}
#' @param name string
#' @param description optional string
#' @param opt_out_link optional link
#' @return custom audience ID
#' @export
fbad_create_audience <- function(fbacc, name, description, opt_out_link) {

    fbad_check_fbacc(fbacc)
    if (missing(name))
        stop('The custom audience name is required.')

    flog.info(paste('Creating new custom audience:', name))

    ## params
    params <- list(access_token = fbacc$access_token, name = name)
    if (!missing(description)) {
        params$description <- description
    }
    if (!missing(opt_out_link)) {
        params$opt_out_link <- opt_out_link
    }

    ## get results
    res <- fbad_request(
        path   = paste0('act_', fbacc$account_id, '/customaudiences'),
        method = "POST",
        params = params)

    ## return ID
    fromJSON(res)$id

}


#' Read metadata on a FB custom audience
#' @references https://developers.facebook.com/docs/marketing-api/custom-audience-targeting/v2.2#read
#' @param fbacc FB_Ad_account object returned by \code{fbad_init}
#' @param audience_id numeric
#' @param fields character vector of fields to be returned
#' @return custom audience ID
#' @export
fbad_read_audience <- function(fbacc, audience_id, fields = c('id', 'account_id', 'approximate_count','data_source', 'delivery_status', 'lookalike_audience_ids', 'lookalike_spec', 'name', 'permission_for_actions', 'operation_status', 'subtype', 'time_updated')) {

    ## get fields
    fields <- match.arg(fields, several.ok = TRUE)
    fields <- paste(fields, collapse = ',')

    fbad_check_fbacc(fbacc)
    if (missing(audience_id))
        stop('A custom audience id is required.')

    ## get results
    res <- fbad_request(
        path   = paste0(audience_id, '?fields=', fields),
        method = "GET",
        params = list(access_token = fbacc$access_token))

    ## return
    fromJSON(res)

}


#' Delete a FB custom audience
#' @references https://developers.facebook.com/docs/marketing-api/custom-audience-targeting/v2.2#delete
#' @param fbacc FB_Ad_account object returned by \code{fbad_init}
#' @param audience_id numeric
#' @return custom audience ID
#' @export
fbad_delete_audience <- function(fbacc, audience_id) {

    fbad_check_fbacc(fbacc)
    if (missing(audience_id))
        stop('A custom audience id is required.')

    stop('This is untested code.')

    ## get results
    res <- fbad_request(
        path   = paste0(audience_id),
        method = "DELETE",
        params = list(access_token = fbacc$access_token))

    ## return
    fromJSON(res)

}


#' Share a FB custom audience with other accounts
#' @references https://developers.facebook.com/docs/marketing-api/custom-audience-targeting/v2.2#sharing
#' @param fbacc FB_Ad_account object returned by \code{fbad_init}
#' @param audience_id audience ID
#' @param adaccounts numeric vector of FB account IDs
#' @note This throws error if you provide wrong account ids OR even valid account ids that were previously granted access to the given custom audience.
#' @export
fbad_share_audience <- function(fbacc, audience_id, adaccounts) {

    fbad_check_fbacc(fbacc)

    flog.info(paste('Sharing', audience_id, 'custom audience ID with', length(adaccounts), 'accounts.'))

    ## make sure adaccounts are integers
    adaccounts <- as.integer64(adaccounts)

    res <- fbad_request(
        path   = paste(audience_id, 'adaccounts', sep = '/'),
        method = "POST",
        params = list(access_token = fbacc$access_token, adaccounts = toJSON(adaccounts)))

}


#' FB add people to audience
#' @references https://developers.facebook.com/docs/marketing-api/custom-audience-targeting/v2.2#create
#' @param fbacc FB_Ad_account object returned by \code{fbad_init}
#' @param audience_id string
#' @param schema only two schema are supported out of the four: you can add persons to a custom audience by e-mail addresses or phone numbers
#' @param hashes character vector of e-mail addresses or phone numbers to be transformed to hashes
#' @export
fbad_add_audience <- function(fbacc, audience_id,
                              schema = c('EMAIL', 'PHONE'),
                              hashes) {

    fbad_check_fbacc(fbacc)

    flog.info(paste('Adding', length(hashes), schema, 'to', audience_id, 'custom audience ID.'))

    if (length(hashes) == 0) {

        warning('Nothing to send to FB')

    } else {

        ## compute hashes for e-mail or phone numbers
        hashes <- sapply(hashes, digest, serialize = FALSE, algo = 'sha256', USE.NAMES = FALSE)

        ## split hashes into 10K groups
        hashes <- split(hashes, 1:length(hashes) %/% 1e4)

        ## get results
        sapply(hashes, function(hash)
            fbad_request(
                path   = paste(audience_id, 'users', sep = '/'),
                method = "POST",
                params = list(
                    access_token = fbacc$access_token,
                    payload      = toJSON(c(
                        list(schema = unbox(paste0(schema, '_SHA256'))),
                        list(data   = hash))))))

    }

    ## TODO parse results and error handling

}


#' Create a new FB lookalike audience similar to an already existing custom audience
#' @references https://developers.facebook.com/docs/marketing-api/lookalike-audience-targeting/v2.2#create
#' @param fbacc FB_Ad_account object returned by \code{fbad_init}
#' @param name string
#' @param origin_audience_id numeric ID of origin custom audience
#' @param ratio Between 0.01-0.20 and increments of 0.01. Indicates the top \code{ratio} percent of original audience in the selected country
#' @param country Country name - the country to find the lookalike people.
#' @return lookalike audience ID
#' @export
fbad_create_lookalike_audience <- function(fbacc, name, origin_audience_id, ratio = 0.01, country = 'US') {

    fbad_check_fbacc(fbacc)
    if (missing(name))
        stop('A custom name for the lookalike audience is required.')
    if (missing(origin_audience_id))
        stop('The origin custom audience id is required.')

    flog.info(paste0('Creating new lookalike (', ratio*100, '%%) ', country, ' audience based on ', origin_audience_id, ': ', name))

    ## get results
    res <- fbad_request(
        path   = paste0('act_', fbacc$account_id, '/customaudiences'),
        method = "POST",
        params = list(
            access_token       = fbacc$access_token,
            name               = name,
            origin_audience_id = origin_audience_id,
            lookalike_spec     = toJSON(list(
                ratio   = ratio,
                country = country
                ), auto_unbox = TRUE)))

    ## return ID
    fromJSON(res)$id

}


#' Query for reach estimate for given targeting spec
#' @param fbacc FB_Ad_account object returned by \code{fbad_init}
#' @param targeting_spec lists of targeting spec characteristics
#' @param currency string
#' @return list
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reachestimate/v2.2}
#' @examples \dontrun{
#' targetspec <- list(
#'   age_min = unbox(24),
#'   age_max = unbox(55),
#'   geo_locations = list(countries = 'US'))
#' }
fbad_reachestimate <- function(fbacc, targeting_spec, currency = 'USD') {

    fbad_check_fbacc(fbacc)
    if (missing(targeting_spec) | !is.list(targeting_spec))
        stop('An R list targetspec is required.')

    ## get results
    res <- fbad_request(
        path   = paste0('act_', fbacc$account_id, '/reachestimate'),
        method = "GET",
        params = list(
            access_token       = fbacc$access_token,
            currency           = currency,
            targeting_spec     = toJSON(targeting_spec)))

    ## parse JSON
    res <- fromJSON(res)

    ## error handling
    if (isTRUE(res$unsupported) | isTRUE(res$bid_estimations$unsupported) | isTRUE(res$data$bid_estimations$unsupported))  {
        stop('Invalid parameters provided, check your targeting_spec.')
    }

    ## return
    c(res['users'], res$bid_estimations[-(1:2)])

}
