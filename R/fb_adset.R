#' Create Ad Set
#' @inheritParams fbad_request
#' @param name name of the Ad Set
#' @param optimization_goal optimization goal
#' @param billing_event the billing event
#' @param is_autobid logical. If \code{TRUE}, autobid is set and you do not need to specify \code{bid_amount}
#' @param bid_amount integer
#' @param promoted_object see at \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign/promoted-object}
#' @param campaign_id parent Ad Campaign id
#' @param status Ad Set status
#' @param daily_budget using account currency
#' @param lifetime_budget using account currency
#' @param end_time UTC UNIX timestamp
#' @param start_time UTC UNIX timestamp
#' @param targeting list
#' @param ... further arguments passed to the API endpoint
#' @return Ad Set id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#Creating}
fbad_create_adset <- function(fbacc,
                              name,
                              optimization_goal = c('NONE', 'APP_INSTALLS', 'CLICKS', 'ENGAGED_USERS', 'EXTERNAL', 'EVENT_RESPONSES', 'IMPRESSIONS', 'LINK_CLICKS', 'OFFER_CLAIMS', 'OFFSITE_CONVERSIONS', 'PAGE_ENGAGEMENT', 'PAGE_LIKES', 'POST_ENGAGEMENT', 'REACH', 'SOCIAL_IMPRESSIONS', 'VIDEO_VIEWS'),
                              billing_event = c('APP_INSTALLS', 'CLICKS', 'IMPRESSIONS', 'LINK_CLICKS', 'OFFER_CLAIMS', 'PAGE_LIKES', 'POST_ENGAGEMENT', 'VIDEO_VIEWS'),
                              is_autobid = FALSE, bid_amount,
                              promoted_object,
                              campaign_id,
                              status = c('ACTIVE', 'PAUSED', 'ARCHIVED', 'DELETED'),
                              daily_budget, lifetime_budget,
                              end_time, start_time,
                              targeting,
                              ...) {

    fbacc <- fbad_check_fbacc()

    optimization_goal <- match.arg(optimization_goal)
    billing_event     <- match.arg(billing_event)

    ## update args for the first or selected value
    status <- match.arg(status)

    ## match call for future reference
    mc <- match.call()

    ## we need a name
    if (missing(name)) {
        stop('Ad Set name is required.')
    }

    ## we need a campaign_group_id
    if (missing(campaign_id)) {
        stop('A campaign ad ID is required.')
    }

    ## verify that we have targeting info
    if (missing(targeting)) {
        stop('A targeting spec is required.')
    }

    ## build base params list
    params <- list(
        name              = name,
        optimization_goal = optimization_goal,
        billing_event     = billing_event,
        campaign_id       = campaign_id,
        configured_status = match.arg(status))
    ## option for auto bidding
    if (is_autobid) {
        params$is_autobid <- TRUE
    } else {
        params$bid_amount <- bid_amount
    }

    ## end_time for lifetime budget
    if (!missing(lifetime_budget) && missing(end_time)) {
        stop('End time of the ad set is required when using a lifetime budget.')
    }

    ## we need a budget
    if (missing(daily_budget) && missing(lifetime_budget)) {
        stop('Either a lifetime_budget or a daily_budget must be set.')
    }
    if (!missing(daily_budget) && !missing(lifetime_budget)) {
        stop('Only one of lifetime_budget or daily_budget should be set.')
    }
    if ((!missing(daily_budget) && !is.numeric(daily_budget)) ||
        !missing(lifetime_budget) && !is.numeric(lifetime_budget)) {
        stop('Numeric value needed for the budget.')
    }
    if (missing(daily_budget)) {
        params$lifetime_budget <- lifetime_budget
        params$end_time        <- end_time
    } else {
        params$daily_budget    <- daily_budget
    }

    ## promoted object based on parent campaign
    campaign <- fbad_read_campaign(fbacc, campaign_id,
                                   fields = 'objective')
    if (campaign$objective %in% c('WEBSITE_CONVERSIONS', 'PAGE_LIKES', 'OFFER_CLAIMS', 'MOBILE_APP_INSTALLS', 'CANVAS_APP_INSTALLS', 'MOBILE_APP_ENGAGEMENT', 'CANVAS_APP_ENGAGEMENT') && missing(promoted_object)) {
        stop(paste('A promoted object is needed when having the objective of', campaign$objective, 'in the parent ad campaign.'))
    }

    ## start time if provided
    if (!missing(start_time)) {
        params$start_time <- start_time
    }

    ## transform lists to JSON
    params$targeting <- toJSON(targeting, auto_unbox = TRUE)

    ## get results
    res <- fbad_request(fbacc,
                        path   = paste0('act_', fbacc$account_id, '/adsets'),
        method = "POST",
        params = params)

    ## return campaign ID on success
    fromJSONish(res)$id

}


#' Read Ad Set details
#' @inheritParams fbad_read_ad
#' @param id ad set id(s)
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#Reading}
fbad_read_adset <- fbad_read_ad


#' Update Ad Set
#' @inheritParams fbad_update_ad
#' @param id Ad Set id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#Updating}
fbad_update_adset <- fbad_update_ad


#' List all Ad Sets for current account or Ad Campaign(s)
#' @inheritParams fbad_list_ad
#' @param id will do the look-up for all Ads based on this ID. Defaults to current FB account. Can be a (vector of) Ad Campaign id(s).
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#Reading}
fbad_list_adset <- fbad_list_ad
