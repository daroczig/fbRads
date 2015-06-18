#' Created ad campaign
#' @param fbacc
#' @param buying_type
#' @param campaign_group_status
#' @param execution_options
#' @param name
#' @param objective
#' @param spend_cap
#' @return ad campaign id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/adcampaign/v2.3#create}
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

    fbad_check_fbacc(fbacc)
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
    res <- fbad_request(
        path   = paste0('act_', fbacc$account_id, '/adcampaign_groups'),
        method = "POST",
        params = c(params, list(access_token = fbacc$access_token)))

    ## return campaign ID on success
    fromJSON(res)$id

}


#' Read ad campaign
#' @param fbacc
#' @param id
#' @param fields
#' @return list
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/adcampaign/v2.3$read}
fbad_read_campaign <- function(fbacc, id, fields = c('id', 'account_id', 'adgroups', 'buying_type', 'campaign_group_status', 'objective', 'spend_cap', 'name')) {

    ## get fields
    fields <- match.arg(fields, several.ok = TRUE)
    fields <- paste(fields, collapse = ',')

    fbad_check_fbacc(fbacc)
    if (missing(id))
        stop('A campaign id is required.')

    ## get results
    res <- fbad_request(
        path   = paste0(id, '?fields=', fields),
        method = "GET",
        params = list(access_token = fbacc$access_token))

    ## return
    fromJSON(res)

}

## TODO: update, delete, stats
