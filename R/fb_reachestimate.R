#' Query for reach estimate for given targeting spec
#' @param fbacc FB_Ad_account object returned by \code{fbad_init}
#' @param targeting_spec lists of targeting spec characteristics
#' @param currency string
#' @return list
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reachestimate/v2.3}
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
