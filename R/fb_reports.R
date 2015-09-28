#' Get Ad Report Stats
#' @inheritParams fbad_request
#' @param ... named arguments passed to the API, like time range, fields, filtering etc.
#' @references \url{https://developers.facebook.com/docs/marketing-api/adreportstats/v2.3}
#' @note This API endpoint is be soon deprecated and only available with v2.3. Use the new Insight feature, if possible.
#' @return data.frame
#' @export
fb_reportstats_ad <- function(fbacc, ...) {

    warning('FB Graph API v2.3 to be deprecated in a few days! Change to the Insights API endpoint via the fb_insights function ASAP.')

    fbacc <- fbad_check_fbacc()

    if (fbacc$api_version > '2.3') {
        stop('Old statistics APIs are being deprecated in favor of Insights Edge introduced with v2.4 of the Facebook Marketing API.')
    }

    ## get report
    res <- fbad_request(fbacc,
        path   = paste0(fbacc$acct_path, 'reportstats'),
        method = 'GET',
        params = list(...))

    ## parse JSON
    res <- fromJSON(res)

    ## save data as list
    l <- list(res$data)

    ## get all pages (if any)
    while (!is.null(res$paging$'next')) {
        res <- fromJSON(getURL(res$paging$'next'))
        l   <- c(l, list(res$data))
    }

    ## return data.frame
    do.call(rbind, l)

}


#' Get Ad Stats
#' @inheritParams fbad_request
#' @param ids adgroup ids
#' @references \url{https://developers.facebook.com/docs/marketing-api/adstatistics/v2.3#read}
#' @note This API endpoint is be soon deprecated and only available with v2.3. Use the new Insight feature, if possible.
#' @return list (raw parsed JSON) to be post-processed
#' @examples \dontrun{
#' res <- fb_stats_ad(...)
#' library(rlist)
#' list.stack(list.select(res, adgroup_id, impressions, clicks, spent, unique_impressions))
#' }
#' @export
fb_stats_ad <- function(fbacc, ids) {

    warning('FB Graph API v2.3 to be deprecated in a few days! Change to the Insights API endpoint via the fb_insights function ASAP.')

    fbacc <- fbad_check_fbacc()

    if (fbacc$api_version > '2.3') {
        stop('Old statistics APIs are being deprecated in favor of Insights Edge introduced with v2.4 of the Facebook Marketing API.')
    }

    ## get report
    res <- fbad_request(fbacc,
        path   = 'stats',
        method = 'GET',
        params = list(ids = paste(ids, collapse = ',')))

    ## parse JSON
    fromJSON(res, flatten = TRUE)

}
