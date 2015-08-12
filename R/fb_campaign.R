#' Created Ad Campaign
#' @inheritParams fbad_request
#' @param buying_type
#' @param campaign_group_status
#' @param execution_options
#' @param name
#' @param objective
#' @param spend_cap
#' @return Ad Campaign id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/adcampaign/v2.4#create}
fbad_create_campaign <- function(fbacc, buying_type = c('AUCTION', 'FIXED_CPM', 'RESERVED'),
                                 campaign_group_status = c('ACTIVE', 'PAUSED'),
                                 execution_options = NULL, name,
                                 objective = c(
                                     'NONE', 'CANVAS_APP_ENGAGEMENT', 'CANVAS_APP_INSTALLS',
                                     'EVENT_RESPONSES', 'LOCAL_AWARENESS', 'MOBILE_APP_ENGAGEMENT',
                                     'MOBILE_APP_INSTALLS', 'OFFER_CLAIMS', 'PAGE_LIKES',
                                     'POST_ENGAGEMENT', 'VIDEO_VIEWS', 'WEBSITE_CLICKS',
                                     'WEBSITE_CONVERSIONS'),
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

    ## drop NULL args
    params <- as.list(unlist(params, recursive = FALSE))

    ## get results
    res <- fbad_request(fbacc,
        path   = paste0('act_', fbacc$account_id, '/adcampaign_groups'),
        method = "POST",
        params = params)

    ## return Ad Campaign ID on success
    fromJSON(res)$id

}


#' Read Ad Campaign details
#' @inheritParams fbad_read_ad
#' @param id Ad Campaign id(s)
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/adcampaign/v2.4$read}
fbad_read_campaign <- fbad_read_ad


#' Update Ad Campaign
#' @inheritParams fbad_update_ad
#' @param id Ad Campaign id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign-group#Updating}
fbad_update_campaign <- fbad_update_ad


#' List all Ad Campaigns for current account
#' @inheritParams fbad_list_ad
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign-group#Reading}
fbad_list_campaign <- fbad_list_ad
