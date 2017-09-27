#' Create ad
#' @inheritParams fbad_request
#' @param name Ad group name
#' @param adset_id Ad Set id
#' @param creative_id creative ID
#' @param status initial status of the Ad group
#' @param ... further parameters passed to the Facebook API
#' @return ad id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/adgroup#Creating}
fbad_create_ad <- function(fbacc,
                           name,
                           adset_id,
                           creative_id,
                           adgroup_status = c('ACTIVE', 'PAUSED'),
                           status = c('ACTIVE', 'PAUSED'),...) {

    fbacc <- fbad_check_fbacc()
    stopifnot(!missing(name),
              !missing(campaign_id) | !missing(adset_id),
              !missing(creative_id))

    ## initial status of the ad to be created
    status <- match.arg(status)

    ## build params list
    params <- list(
        name     = name,
        creative = toJSON(list(creative_id = unbox(creative_id))),
        adset_id = adset_id,
        status   = status)

    ## add further params if provided
    if (length(list(...)) > 0) {
        params <- c(params, list(...))
    }

    ## get results
    res <- fbad_request(fbacc,
        path   = paste0('act_', fbacc$account_id, '/ads'),
        method = "POST",
        params = params)

    ## return campaign ID on success
    fromJSONish(res)$id

}


#' Read ad details
#' @inheritParams fbad_request
#' @param id ad id(s)
#' @param fields character vector of fields to get from the API, defaults to \code{id}. Please refer to the Facebook documentation for a list of possible values.
#' @param simplify return \code{data.frame} or \code{list}
#' @return data.frame
#' @note Will do a batched request to the Facebook API if multiple ids are provided.
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/adgroup#Reading}
#' @examples \dontrun{
#' ## get and Ad ID from yesterday
#' adid <- fb_insights(date_preset = 'yesterday', level = 'ad')[[1]]$ad_id[1]
#' ## look for current status of the Ad
#' fbad_read_ad(id = adid, fields = c('effective_status'))
#' }
#' @importFrom data.table rbindlist setDF
fbad_read_ad <- function(fbacc, id, fields = 'id', simplify = TRUE) {

    fbacc <- fbad_check_fbacc()

    ## merge fields
    fields <- paste(fields, collapse = ',')

    ## we need at least one ad id
    if (missing(id)) {
        stop('Please provide at least one ad id.')
    }

    ## get results for only one id
    if (length(id) == 1) {
        res <- fbad_request(fbacc,
                            path   = id,
                            params = list(fields = fields),
                            method = "GET")
        res <- fromJSONish(res)
        if (simplify) {
            res <- as.data.frame(res, stringsAsFactors = FALSE)
        }
        return(res)
    }

    ## or do batched query
    res <- lapply(
        split(id, 1:length(id) %/% 50),
        function(batch) {

            ## query FB by 50 ids at a time
            res <- fbad_request(fbacc,
                                path   = '',
                                params = list(
                                    ids    = paste(batch, collapse = ','),
                                    fields = fields),
                                method = "GET")

            res <- fromJSONish(res)
            if (simplify) {
                res <- rbindlist(lapply(res, as.data.frame, stringsAsFactors = FALSE), fill = TRUE)
            }
            res

        })

    ## return
    if (simplify) {
        res <- setDF(rbindlist(res, fill = TRUE))
    }
    res

}


#' Preview ad
#' @inheritParams fbad_request
#' @param id ad id(s)
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/generatepreview}
fbad_preview_ad <- function(fbacc, id, ad_format = c(
                                           'DESKTOP_FEED_STANDARD', 'RIGHT_COLUMN_STANDARD',
                                           'MOBILE_FEED_STANDARD', 'MOBILE_BANNER', 'MOBILE_INTERSTITIAL',
                                           'INSTAGRAM_STANDARD')) {

    fbacc <- fbad_check_fbacc()
    ad_format <- match.arg(ad_format)
    fromJSONish(fbad_request(fbacc,
                          path   = file.path(id, 'previews'),
                          params = list(ad_format = ad_format),
                          method = "GET"))$data$body

}


#' Update ad
#' @inheritParams fbad_request
#' @param id ad id
#' @param ... parameters passed to Facebook API
#' @return invisible TRUE
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/adgroup#Updating}
fbad_update_ad <- function(fbacc, id, ...) {

    fbacc <- fbad_check_fbacc()

    ## we need one and only one id
    if (missing(id)) {
        stop('Please provide at least one ad id.')
    }

    ## query Facebook API
    res <- fbad_request(fbacc,
                        path   = id,
                        params = list(...),
                        method = "POST")

    ## success
    invisible(fromJSONish(res)$success)

}


#' List all Ads for current account, list of Ad Sets or Campaigns
#' @inheritParams fbad_request
#' @param id will do the look-up for all Ads based on this ID. Defaults to current FB account. Can be a (vector of) Ad Set or Campaign id(s).
#' @param statuses character vector of statuses to use as a filter. Defaults to none. Please refer to the Facebook documentation for a list of possible values.
#' @param fields character vector of fields to get from the API, defaults to \code{id}. Please refer to the Facebook documentation for a list of possible values.
#' @param simplify boolean whether response is simplified to data.frame or else returned as raw list
#' @return data.frame
#' @note Will do a batched request to the Facebook API if multiple ids are provided.
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/adgroup#read-adaccount}
fbad_list_ad <- function(fbacc, id, statuses, fields = 'id', simplify = TRUE) {

    fbacc <- fbad_check_fbacc()

    ## lookup caller fn name
    fn <- this_function_name()

    ## merge fields
    fields <- paste(fields, collapse = ',')

    ## basic params
    params <- list(fields = fields, limit = 1000)

    ## filter for status
    if (!missing(statuses)) {
        params$effective_status <- toJSON(statuses)
    }

    ## default ID for current Ad Account
    if (missing(id) | fn == 'fbad_list_campaign') {
        id <- paste0('act_', fbacc$account_id)
    }

    ## API endpoint
    endpoint <- switch(fn,
                       'fbad_list_ad'       = 'ads',
                       'fbad_list_adset'    = 'adsets',
                       'fbad_list_campaign' = 'campaigns',
                       'fbad_list_audience' = 'customaudiences')

    ## paged query for one id
    if (length(id) == 1) {
        ## get first page with the list of (max) 1,000 ads
        res <- fbad_request(fbacc,
                            path   = file.path(id, endpoint),
                            params = params,
                            method = "GET")

        ## parse JSON
        res <- fromJSONish(res)

        ## save data as list
        l <- list(res$data)

        ## get all pages (if any)
        while (!is.null(res$paging$'next')) {
            url <- res$paging$`next`
            url <- url_parse(url)
            res <- fbad_request(path = url$path, method = 'GET', params = url$params)
            res <- fromJSONish(res)
            l   <- c(l, list(res$data))
        }

        ## return data.frame if simplify is true
        if (simplify) {
          l <- do.call(rbind, l)
        }
        return(l)
    }

    ## batched query for multiple ids (no need for paging)
    res <- lapply(
        split(id, 1:length(id) %/% 50),
        function(batch) {

            ## query FB by 50 ids at a time
            res <- fbad_request(fbacc,
                                path   = '',
                                params = list(
                                    batch  = toJSON(
                                        data.frame(method = 'GET',
                                                   relative_url = paste0(
                                                       'v', fbacc$api_version,
                                                       '/', batch, '/', endpoint,
                                                       '?fields=', fields))
                                        )),
                                method = 'POST')

            ## transform data part of the list to data.frame
            do.call(rbind, lapply(fromJSONish(res)$body,
                                  function(x) fromJSONish(x)$data))

        })

    ## return merged data.frame
    res <- do.call(rbind, res)
    rownames(res) <- NULL
    res

}
