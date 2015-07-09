#' Get Ad Report Stats
#' @param fbacc
#' @param ... named arguments passed to the API, like time range, fields, filtering etc.
#' @references \url{https://developers.facebook.com/docs/marketing-api/adreportstats/v2.3}
#' @note This will be soon deprecated. Use the new Insight feature, if possible.
#' @return data.frame
#' @export
fb_reportstats_ad <- function(fbacc, ...) {

    ## start sync or async report generation
    res <- fbad_request(
        path   = paste0(fbacc$acct_path, 'reportstats'),
        method = 'GET',
        params = list(access_token = fbacc$access_token,
            ...))

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
