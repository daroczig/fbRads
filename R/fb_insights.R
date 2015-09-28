#' Insights
#' @inheritParams fbad_request
#' @param target ad account id (default), campaign id, adset id or ad id
#' @param job_type synchronous or asynchronous request. If the prior fails with "please reduce the amount of data", it will fall back to async request.
#' @param ... named arguments passed to the API, like time range, fields, filtering etc.
#' @references \url{https://developers.facebook.com/docs/marketing-api/insights/v2.4}
#' @return list
#' @export
#' @examples \dontrun{
#' fb_insights(fbacc)
#'
#' ## process results
#' l <- fb_insights(fbacc, date_preset = 'today', level = 'adgroup')
#' library(rlist)
#' list.stack(list.select(l, date_start, date_stop, adgroup_id, total_actions, total_unique_actions, total_action_value, impressions, unique_impressions, social_impressions, unique_social_impressions, clicks, unique_clicks, social_clicks, unique_social_clicks, spend, frequency, deeplink_clicks, app_store_clicks, website_clicks, reach, social_reach, ctr, unique_ctr, cpc, cpm, cpp, cost_per_total_action, cost_per_unique_click, relevance_score = relevance_score$score))
#' }
fb_insights <- function(fbacc, target = fbacc$acct_path, job_type = c('sync', 'async'), ...) {

    fbacc <- fbad_check_fbacc()

    ## save call for possible future evaluation
    mc <- match.call()

    ## update args
    job_type <- match.arg(job_type)

    ## start sync or async report generation
    res <- tryCatch(fbad_request(fbacc,
        path   = file.path(sub('/$', '', target), 'insights'),
        method = switch(job_type,
            'sync'  = 'GET',
            'async' = 'POST'),
        params = list(...), log = FALSE), error = function(e) e)

    ## sync request
    if (job_type == 'sync') {

        ## if it was a sync job and failed
        if (inherits(res, 'error')) {

            ## let's try an async query for larger data
            if (res$message == "Please reduce the amount of data you're asking for, then retry your request") {
                flog.debug('Sync request failed, starting async request.')
                mc$job_type <- 'async'
                return(eval(mc))
            }

            stop(res$message)

        }

    ## async request
    } else {

        ## we have an async job, we need the job ID
        id <- fromJSON(res)[[1]]

        ## get results
        res <- fbad_insights_get_async_results(id = id)

    }

    ## parse JSON
    res <- fromJSON(res)

    ## save data as list
    l <- list(res$data)

    ## get all pages (if any)
    while (!is.null(res$paging$'next')) {
        res <- fromJSON(getURL(res$paging$'next'))
        l   <- c(l, list(res$data))
    }

    ## return list
    l

}


#' Wait for and get asynchronous report results
#' @inheritParams fbad_request
#' @param id job ID
#' @return JSON
#' @keywords internal
fbad_insights_get_async_results <- function(fbacc, id) {

    fbacc <- fbad_check_fbacc()

    ## get status
    res <- fbad_request(fbacc,
        path   = id,
        method = "GET")

    ## parse JSON
    res <- fromJSON(res)

    ## default polling interval (in seconds)
    wait_time  <- 2/5
    ## and percentage
    percentage <- res$async_percent_completion

    ## job still running
    while (res$async_status %in% c('Job Not Started', 'Job Started', 'Job Running')) {

        ## update polling interval
        dpercentage <- res$async_percent_completion - percentage
        wait_time   <- wait_time * ifelse(dpercentage > 10,
                                          ifelse(dpercentage > 25, 0.5,
                                                 ifelse(dpercentage > 15, 0.75, 1)),
                                          ifelse(dpercentage > 5, 2, 5))
        percentage  <- res$async_percent_completion

        ## log
        flog.debug(paste0(id, ' Async ',
                          res$async_status, ' (',
                          res$async_percent_completion,
                          '%). Waiting ', round(wait_time, 1), ' seconds...'))

        ## wait a bit
        Sys.sleep(wait_time)

        ## instead of a recursive call, let's specify the query again
        ## as nested calls was likely to cause segfault in R :(
        res <- fromJSON(fbad_request(fbacc,
            path   = id,
            method = "GET"))

    }

    ## job completed
    if (res$async_status == 'Job Completed') {

        ## get the report
        return(fbad_request(fbacc,
            path   = file.path(id, 'insights'),
            method = "GET"))

    }

    ## error?
    flog.error(toJSON(res))
    stop('Unexpected response for the asynchronous job.')

}
