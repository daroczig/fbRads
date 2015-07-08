#' Insights
#' @param fbacc
#' @param target ad account id (default), campaign id, adset id or ad id
#' @param job_type synchronous or asynchronous request. If the prior fails with "please reduce the amount of data", it will fall back to async request.
#' @param ... named arguments passed to the API, like time range, fields, filtering etc.
#' @references \url{https://developers.facebook.com/docs/marketing-api/insights/v2.3}
#' @return JSON
#' @export
#' @examples \dontrun{
#' fb_insights(fbacc)
#' fb_insights(fbacc, date_preset = 'today', level = 'adgroup')
#' }
fb_insights <- function(fbacc, target = fbacc$acct_path, job_type = c('sync', 'async'), ...) {

    ## save call for possible future evaluation
    mc <- match.call()

    ## update args
    job_type <- match.arg(job_type)

    ## start sync or async report generation
    res <- tryCatch(fbad_request(
        path   = file.path(sub('/$', '', target), 'insights?date_preset=last_30_days'),
        method = switch(job_type,
            'sync'  = 'GET',
            'async' = 'POST'),
        params = list(access_token = fbacc$access_token,
            ...), log = FALSE), error = function(e) e)

    if (job_type == 'sync') {

        ## if it was a sync job and failed
        if (inherits(res, 'error')) {

            ## let's try an async query for larger data
            if (res$message == "Please reduce the amount of data you're asking for, then retry your request") {
                mc$job_type <- 'async'
                return(eval(mc))
            }

            stop(res$message)

        }

        ## otherwise we have the results
        return(res)

    }

    ## we have an async job, we need the job ID
    id <- fromJSON(res)[[1]]

    ## get results
    res <- fbad_insights_get_async_results(id)

    ## return
    res

}


#' Wait for and get asynchronous report results
#' @param id job ID
#' @return JSON
#' @keywords internal
fbad_insights_get_async_results <- function(id) {

    mc <- match.call()

    ## get status
    res <- fbad_request(
        path   = id,
        method = "GET",
        params = list(access_token = fbacc$access_token))

    ## parse JSON
    res <- fromJSON(res)

    ## job still running
    if (res$async_status %in% c('Job Not Started', 'Job Started', 'Job Running')) {
        flog.debug(paste0(res$async_status, ' (',
                          res$async_percent_completion,
                          '%%). Waiting 500ms...'))
        Sys.sleep(0.5)
        return(eval(mc))
    }

    ## job completed
    if (res$async_status == 'Job Completed') {
        return(fbad_request(
            path   = file.path(id, 'insights'),
            method = "GET",
            params = list(access_token = fbacc$access_token)))
    }

    ## error?
    flog.error(toJSON(res))
    stop('Unexpected response for the asynchronous job.')

}
