#' Query for reach estimate for given targeting spec
#' @inheritParams fbad_request
#' @param targeting_spec lists of targeting spec characteristics as described at \url{https://developers.facebook.com/docs/marketing-api/targeting-specs}
#' @param currency string
#' @param optimize_for what are you optimizing for in the planned Ad Set?
#' @return list
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-account/reachestimate/}
#' @examples \dontrun{
#' targetspec <- list(
#'   age_min = unbox(24),
#'   age_max = unbox(55),
#'   geo_locations = list(countries = 'US'))
#' fbad_reachestimate(targeting_spec = targetspec)
#' }
fbad_reachestimate <- function(fbacc, targeting_spec, currency = 'USD',
                               optimize_for = c('NONE', 'APP_INSTALLS', 'CLICKS',
                                                'ENGAGED_USERS', 'EXTERNAL',
                                                'EVENT_RESPONSES', 'IMPRESSIONS',
                                                'LINK_CLICKS', 'OFFER_CLAIMS',
                                                'OFFSITE_CONVERSIONS', 'PAGE_ENGAGEMENT',
                                                'PAGE_LIKES', 'POST_ENGAGEMENT',
                                                'REACH', 'SOCIAL_IMPRESSIONS',
                                                'VIDEO_VIEWS')) {

    fbacc <- fbad_check_fbacc()
    if (missing(targeting_spec) | !is.list(targeting_spec))
        stop('An R list targetspec is required.')

    ## default parameters
    params = list(
        currency       = currency,
        targeting_spec = toJSON(targeting_spec),
        optimize_for   = match.arg(optimize_for))

    ## get results
    res <- fbad_request(fbacc,
        path   = paste0('act_', fbacc$account_id, '/reachestimate'),
        method = "GET",
        params = params)

    ## parse JSON
    res <- .safeFromJSON(res)

    ## error handling
    if (isTRUE(res$unsupported) | isTRUE(res$bid_estimations$unsupported) | isTRUE(res$data$bid_estimations$unsupported))  {
        stop('Invalid parameters provided, check your targeting_spec.')
    }

    ## v2.5 returns a "data" list
    res <- res$data

    ## return
    c(res['users'], res$bid_estimations[-(1:2)])

}
