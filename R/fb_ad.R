#' Create ad
#' @inheritParams fbad_request
#' @param name Ad group name
#' @param campaign_id Ad Set id
#' @param creative_id creative ID
#' @param adgroup_status initial status of the Ad group
#' @param status initial status of the Ad group
#' @param ... further parameters passed to the Facebook API
#' @return ad id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/adgroup/v2.5#Creating}
fbad_create_ad <- function(fbacc,
                           name, campaign_id, creative_id,
                           adgroup_status = c('ACTIVE', 'PAUSED'),
                           status = c('ACTIVE', 'PAUSED'),...) {

    fbacc <- fbad_check_fbacc()
    stopifnot(!missing(name), !missing(campaign_id), !missing(creative_id))

    ## initial status of the ad to be created
    status <- match.arg(status)
    if (!missing(adgroup_status)) {
        warning('"adgroup_status" argument is deprecated, use "status" instead from v2.5')
        status <- match.arg(adgroup_status)
    }

    ## build params list
    params <- list(
        name           = name,
        campaign_id    = campaign_id,
        creative       = toJSON(list(creative_id = unbox(creative_id))))

    ## add status for v2.4 VS v2.5
    params <- c(params, ifelse(fb_api_version() < '2.5',
                               list(adgroup_status = status),
                               list(status = status)))

    ## add further params if provided
    if (length(list(...)) > 0) {
        params <- c(params, list(...))
    }

    ## get results
    res <- fbad_request(fbacc,
        path   = paste0('act_', fbacc$account_id,
                        ifelse(fb_api_version() < '2.5', '/adgroups', '/ads')),
        method = "POST",
        params = params)

    ## return campaign ID on success
    fromJSON(res)$id

}


#' Read ad details
#' @inheritParams fbad_request
#' @param id ad id(s)
#' @param fields character vector of fields to get from the API, defaults to \code{id}. Please refer to the Facebook documentation for a list of possible values.
#' @return data.frame
#' @note Will do a batched request to the Facebook API if multiple ids are provided.
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/adgroup/v2.5#Reading}
#' @examples \dontrun{
#' ## get and Ad ID from yesterday
#' adid <- fb_insights(date_preset = 'yesterday', level = 'ad')[[1]]$ad_id[1]
#' ## look for current status of the Ad
#' fbad_read_ad(id = adid, fields = c('effective_status'))
#' }
fbad_read_ad <- function(fbacc, id, fields = 'id') {

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
        return(as.data.frame(fromJSON(res), stringsAsFactors = FALSE))
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

            ## transform to data.frame
            do.call(rbind, lapply(fromJSON(res),
                                  as.data.frame, stringsAsFactors = FALSE))

        })

    ## return
    do.call(rbind, res)

}


#' Update ad
#' @inheritParams fbad_request
#' @param id ad id
#' @param ... parameters passed to Facebook API
#' @return invisible TRUE
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/adgroup/v2.5#Updating}
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
    invisible(fromJSON(res)$success)

}


#' List all Ads for current account, list of Ad Sets or Campaigns
#' @inheritParams fbad_request
#' @param id will do the look-up for all Ads based on this ID. Defaults to current FB account. Can be a (vector of) Ad Set or Campaign id(s).
#' @param statuses character vector of statuses to use as a filter. Defaults to none. Please refer to the Facebook documentation for a list of possible values.
#' @param fields character vector of fields to get from the API, defaults to \code{id}. Please refer to the Facebook documentation for a list of possible values.
#' @return data.frame
#' @note Will do a batched request to the Facebook API if multiple ids are provided.
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/adgroup/v2.5#read-adaccount}
fbad_list_ad <- function(fbacc, id, statuses, fields = 'id') {

    fbacc <- fbad_check_fbacc()

    ## lookup caller fn name
    fn <- deparse(match.call()[[1]])

    ## merge fields
    fields <- paste(fields, collapse = ',')

    ## basic params
    params <- list(fields = fields, limit = 1000)

    ## filter for status
    if (!missing(statuses)) {

        params$adgroup_status <- toJSON(statuses)

        ## update filter name for Ad Sets and Campaigns
        if (fn == 'fbad_list_adset') {
            names(params)[3] <- 'campaign_status'
        }
        if (fn == 'fbad_list_campaign') {
            names(params)[3] <- 'campaign_group_status'
        }

    }

    ## default ID for current Ad Account
    if (missing(id) | fn == 'fbad_list_campaign') {
        id <- paste0('act_', fbacc$account_id)
    }

    ## API endpoint
    endpoint <- switch(fn,
                       'fbad_list_ad'       = ifelse(fb_api_version() < '2.5',
                                                     'adgroups', 'ads'),
                       'fbad_list_adset'    = ifelse(fb_api_version() < '2.5',
                                                     'adcampaigns', 'adsets'),
                       'fbad_list_campaign' = ifelse(fb_api_version() < '2.5',
                                                     'adcampaign_groups', 'campaigns'))

    ## paged query for one id
    if (length(id) == 1) {
        ## get first page with the list of (max) 1,000 ads
        res <- fbad_request(fbacc,
                            path   = file.path(id, endpoint),
                            params = params,
                            method = "GET")

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
        return(do.call(rbind, l))
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
            do.call(rbind, lapply(fromJSON(res)$body,
                                  function(x) fromJSON(x)$data))

        })

    ## return merged data.frame
    res <- do.call(rbind, res)
    rownames(res) <- NULL
    res

}
