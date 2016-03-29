#' FB Search API Querying
#' @inheritParams fbad_request
#' @param q string that is being searched for
#' @param type describes the type of search eg: adinterest, adeducationmajor etc
#' @param ... other optional parameters accepted by the endpoint as key = value pairs eg: \code{limit = 5000}.
#' @return \code{data.frame} containing results
#' @examples \dontrun{
#' fbad_get_search(q = 'r programming language', type = 'adinterest')
#' fbad_get_search(q = c('dog', 'cat'), type = 'adinterestvalid')
#' }
#' @references \url{https://developers.facebook.com/docs/marketing-api/targeting-search/v2.5}
#' @export
fbad_get_search <- function(
    fbacc, q,
    type = c(
        'adeducationschool', 'adeducationmajor',
        'adgeolocation', 'adcountry', 'adzipcode', 'adgeolocationmeta', 'adradiussuggestion',
        'adinterest', 'adinterestsuggestion', 'adinterestvalid',
        'adlocale', 'adTargetingCategory', 'adworkemployer'), ... ) {

    type  <- match.arg(type)
    fbacc <- fbad_check_fbacc()

    ## default params
    params <- list(limit        = 500,
                   type         = type,
                   list         = "GLOBAL")

    ## update params
    if (length(list(...)) > 0) {
        params <- c(params, list(...))
    }

    ## Handle term input variation in API
    if (type %in% c("adinterestvalid", "adinterestsuggestion")) {

        params <- c(params, list(interest_list = toJSON((q))))

    } else {

        if (length(q) > 1) {
            warning("Multiple keywords not allowed. Using first")
            q <- q[1]
        }

        params <- c(params, list("q" = as.character(q)))

    }

    ## get results
    properties <- fbad_request(fbacc,
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
