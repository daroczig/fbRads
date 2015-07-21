#' Create adset
#' @param fbacc (optional) \code{FB_Ad_account} object, which defaults to the last returned object of \code{\link{fbad_init}}.
#' @param name
#' @param bid_type
#' @param bid_info
#' @param campaign_group_id
#' @param campaign_status
#' @param daily_budget
#' @param lifetime_budget
#' @param end_time
#' @param start_time
#' @param targeting
#' @return adset id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/adset/v2.3#create}
fbad_create_adset <- function(fbacc,
                              name,
                              bid_type = c('CPC', 'CPM', 'ABSOLUTE_OCPM', 'CPA'),
                              bid_info, campaign_group_id,
                              campaign_status = c('ACTIVE', 'PAUSED', 'ARCHIVED', 'DELETED'),
                              daily_budget, lifetime_budget,
                              end_time, start_time,
                              targeting
                              ) {

    fbacc <- fbad_check_fbacc()

    ## update args for the first or selected value
    bid_type <- match.arg(bid_type)
    campaign_status <- match.arg(campaign_status)

    ## match call for future reference
    mc <- match.call()

    ## we need a name
    if (missing(name)) {
        stop('Adset name is required.')
    }

    ## we need a campaign_group_id
    if (missing(campaign_group_id)) {
        stop('A campaign ad ID is required.')
    }

    ## we need bid info as a list
    if (missing(bid_info) || !is.list(bid_info)) {
        stop('Invalid bid_info provided. Please set a lit related to the bid_type.')
    }

    ## verify that we have targeting info
    if (missing(targeting)) {
        stop('A targeting spec is required.')
    }

    ## build base params list
    params <- list(
        name              = name,
        bid_type          = bid_type,
        campaign_status   = campaign_status,
        campaign_group_id = campaign_group_id)

    ## more detailed bid info checks
    if (bid_type == 'CPC' &&
        (names(bid_info) != 'CLICKS' ||
             !is.numeric(bid_info$CLICKS) ||
             bid_info$CLICKS < 1)) {
        stop('For CPC, you have to specify min. 1 cent for CLICKS.')
    }

    if (bid_type == 'CPM' &&
        (names(bid_info) != 'IMPRESSIONS' ||
             !is.numeric(bid_info$IMPRESSIONS) ||
             bid_info$IMPRESSIONS < 1)) {
        stop('For CPM, you have to specify min. 1 cent for IMPRESSIONS.')
    }

    if (bid_type == 'CPA' &&
        (names(bid_info) != 'ACTIONS' ||
             !is.numeric(bid_info$IMPRESSIONS) ||
             bid_info$IMPRESSIONS < 1)) {
        stop('For CPA, you have to specify min. 1 cent for ACTIONS')
    }

    if (bid_type == 'ABSOLUTE_OCPM' &&
        (length(names(bid_info)) != 4 ||
             !all(names(bid_info) %in% c('ACTIONS', 'REACH', 'CLICKS', 'SOCIAL')) ||
             sum(sapply(bid_info, is.numeric)) != 4 ||
             bid_info$ACTIONS < 1 ||
             bid_info$REACH   < 2 ||
             bid_info$CLICKS  < 1 ||
             bid_info$SOCIAL  < 1)) {
        stop('For ABSOLUTE_OCPM, you have to specify min. 1 cent for ACTIONS, CLICKS, SOCIAL and min. 2 cents for REACH.')
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
    campaign <- fbad_read_campaign(fbacc, campaign_group_id)
    if (campaign$objective %in% c('WEBSITE_CONVERSIONS', 'PAGE_LIKES', 'OFFER_CLAIMS', 'MOBILE_APP_INSTALLS', 'CANVAS_APP_INSTALLS', 'MOBILE_APP_ENGAGEMENT', 'CANVAS_APP_ENGAGEMENT') && is.missing(promoted_object)) {
        stop(paste('A promoted object is needed when having the objective of', campaign$objective, 'in the parent ad campaign.'))
    }

    ## start time if provided
    if (!missing(start_time)) {
        params$start_time <- start_time
    }

    ## transform lists to JSON
    params$targeting <- toJSON(targeting, auto_unbox = TRUE)
    params$bid_info  <- toJSON(bid_info, auto_unbox = TRUE)

    ## get results
    res <- fbad_request(
        path   = paste0('act_', fbacc$account_id, '/adcampaigns'),
        method = "POST",
        params = c(params, list(access_token = fbacc$access_token)))

    ## return campaign ID on success
    fromJSON(res)$id

}
