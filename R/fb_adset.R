#' Create Ad Set
#' @inheritParams fbad_request
#' @param name name of the Ad Set
#' @param optimization_goal optimization goal
#' @param billing_event the billing event
#' @param bid_amount integer
#' @param promoted_object see at \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign/promoted-object/v2.4}
#' @param campaign_group_id parent Ad Campaign id
#' @param campaign_status Ad Set status
#' @param daily_budget in account currency
#' @param lifetime_budget in account currency
#' @param end_time datetime
#' @param start_time datetime
#' @param targeting list
#' @param ... further arguments passed to the API endpoint
#' @return Ad Set id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#Creating}
fbad_create_adset <- function(fbacc,
                              name,
                              ## from v2.4
                              optimization_goal = c('NONE', 'APP_INSTALLS', 'CLICKS', 'ENGAGED_USERS', 'EXTERNAL', 'EVENT_RESPONSES', 'IMPRESSIONS', 'LINK_CLICKS', 'OFFER_CLAIMS', 'OFFSITE_CONVERSIONS', 'PAGE_ENGAGEMENT', 'PAGE_LIKES', 'POST_ENGAGEMENT', 'REACH', 'SOCIAL_IMPRESSIONS', 'VIDEO_VIEWS'),
                              billing_event = c('APP_INSTALLS', 'CLICKS', 'IMPRESSIONS', 'LINK_CLICKS', 'OFFER_CLAIMS', 'PAGE_LIKES', 'POST_ENGAGEMENT', 'VIDEO_VIEWS'),
                              bid_amount,
                              ## end of special params
                              promoted_object,
                              campaign_group_id,
                              campaign_status = c('ACTIVE', 'PAUSED', 'ARCHIVED', 'DELETED'),
                              daily_budget, lifetime_budget,
                              end_time, start_time,
                              targeting, ...) {

    fbacc <- fbad_check_fbacc()

    optimization_goal <- match.arg(optimization_goal)
    billing_event     <- match.arg(billing_event)

    ## update args for the first or selected value
    campaign_status <- match.arg(campaign_status)

    ## match call for future reference
    mc <- match.call()

    ## we need a name
    if (missing(name)) {
        stop('Ad Set name is required.')
    }

    ## we need a campaign_group_id
    if (missing(campaign_group_id)) {
        stop('A campaign ad ID is required.')
    }

    ## verify that we have targeting info
    if (missing(targeting)) {
        stop('A targeting spec is required.')
    }

    ## build base params list
    params <- list(
        name              = name,
        campaign_status   = campaign_status,
        campaign_group_id = campaign_group_id)

    ## version specific params
    params <- c(params, list(
                            optimization_goal = optimization_goal,
                            billing_event     = billing_event,
                            bid_amount        = bid_amount))

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
    campaign <- fbad_read_campaign(fbacc, campaign_group_id)
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
        path   = paste0('act_', fbacc$account_id, '/adcampaigns'),
        method = "POST",
        params = params)

    ## return campaign ID on success
    fromJSON(res)$id

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
