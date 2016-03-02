#' Created Ad Campaign
#' @inheritParams fbad_request
#' @param buying_type Facebook optimization algorithm to delivery, pricing, and limits
#' @param campaign_status initial status of the Ad Campaign (v2.5)
#' @param campaign_group_status initial status of the Ad Campaign (v2.4)
#' @param execution_options special execution settings passed to the API
#' @param name Ad Campaign name
#' @param objective the campaign's objective
#' @param spend_cap spend cap of the campaign
#' @return Ad Campaign id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign-group#Creating}
fbad_create_campaign <- function(fbacc, buying_type = c('AUCTION', 'FIXED_CPM', 'RESERVED'),
                                 campaign_group_status = c('ACTIVE', 'PAUSED'),
                                 campaign_status = c('ACTIVE', 'PAUSED'),
                                 execution_options = NULL, name,
                                 objective,
                                 spend_cap = NULL) {

    fbacc <- fbad_check_fbacc()
    if (missing(name))
        stop('A campaign name is required.')

    buying_type           <- match.arg(buying_type)
    objective             <- match.arg(objective)
    campaign_group_status <- match.arg(campaign_group_status)

    ## build params list
    params <- list(
        buying_type           = buying_type,
        campaign_group_status = campaign_group_status,
        objective             = objective,
        name                  = name,
        execution_options     = execution_options,
        spend_cap             = spend_cap)

    ## version specific params
    if (fb_api_version() < '2.5') {
        params$objective <- match.arg(
            objective,
            c(
                'NONE', 'CANVAS_APP_ENGAGEMENT', 'CANVAS_APP_INSTALLS',
                'EVENT_RESPONSES', 'LOCAL_AWARENESS', 'MOBILE_APP_ENGAGEMENT',
                'MOBILE_APP_INSTALLS', 'OFFER_CLAIMS', 'PAGE_LIKES',
                'POST_ENGAGEMENT', 'VIDEO_VIEWS', 'WEBSITE_CLICKS',
                'WEBSITE_CONVERSIONS'))
    } else {
        ## objectives were changed in v2.5
        ## * WEBSITE_CLICKS -> LINK_CLICKS
        ## * WEBSITE_CONVERSIONS -> CONVERSIONS
        params$objective <- match.arg(
            objective,
            c('BRAND_AWARENESS', 'CANVAS_APP_ENGAGEMENT', 'CANVAS_APP_INSTALLS',
              'CONVERSIONS', 'EVENT_RESPONSES', 'EXTERNAL', 'LEAD_GENERATION',
              'LINK_CLICKS', 'LOCAL_AWARENESS', 'MOBILE_APP_ENGAGEMENT',
              'MOBILE_APP_INSTALLS', 'OFFER_CLAIMS', 'PAGE_LIKES',
              'POST_ENGAGEMENT', 'PRODUCT_CATALOG_SALES', 'VIDEO_VIEWS'))
    }


    ## drop NULL args
    params <- as.list(unlist(params, recursive = FALSE))

    ## get results
    res <- fbad_request(fbacc,
                        path   = paste0('act_', fbacc$account_id,
                                        ifelse(fb_api_version() < '2.5',
                                               '/adcampaign_groups', '/campaigns')),
        method = "POST",
        params = params)

    ## return Ad Campaign ID on success
    fromJSON(res)$id

}


#' Read Ad Campaign details
#' @inheritParams fbad_read_ad
#' @param id Ad Campaign id(s)
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign-group#Reading}
fbad_read_campaign <- fbad_read_ad


#' Update Ad Campaign
#' @inheritParams fbad_update_ad
#' @param id Ad Campaign id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign-group#Updating}
fbad_update_campaign <- fbad_update_ad


#' List all Ad Campaigns for current account
#' @inheritParams fbad_list_ad
#' @param id not supported argument
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign-group#Reading}
fbad_list_campaign <- fbad_list_ad
