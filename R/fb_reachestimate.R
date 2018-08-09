#' Query for reach estimate for given targeting spec
#' @inheritParams fbad_request
#' @param targeting_spec lists of targeting spec characteristics as described at \url{https://developers.facebook.com/docs/marketing-api/targeting-specs}
#' @return list
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-account/reachestimate/}
#' @examples \dontrun{
#' library(jsonlite)
#' targetspec <- list(
#'   age_min = unbox(24),
#'   age_max = unbox(55),
#'   geo_locations = list(countries = 'US'))
#' fbad_reachestimate(targeting_spec = targetspec)
#' }
fbad_reachestimate <- function(fbacc, targeting_spec) {

    fbacc <- fbad_check_fbacc()
    if (missing(targeting_spec) | !is.list(targeting_spec))
        stop('An R list targetspec is required.')

    ## get results
    res <- fbad_request(fbacc,
        path   = paste0('act_', fbacc$account_id, '/reachestimate'),
        method = "GET",
        params = list(targeting_spec = toJSON(targeting_spec)))

    ## parse JSON
    res <- fromJSONish(res)

    ## error handling
    if (isTRUE(res$unsupported) | isTRUE(res$bid_estimations$unsupported) | isTRUE(res$data$bid_estimations$unsupported))  {
        stop('Invalid parameters provided, check your targeting_spec.')
    }

    ## v2.5 returns a "data" list
    res <- res$data

    ## return
    c(res['users'], res$bid_estimations[-(1:2)])

}
