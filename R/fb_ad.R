#' Create ad
#' @param fbacc (optional) \code{FB_Ad_account} object, which defaults to the last returned object of \code{\link{fbad_init}}.
#' @param name
#' @param campaign_id
#' @param creative_id
#' @param adgroup_status
#' @return ad id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/adgroup/v2.3#create}
fbad_create_ad <- function(fbacc,
                                 name, campaign_id, creative_id,
                                 adgroup_status = c('ACTIVE', 'PAUSED')
                                 ) {

    fbacc <- fbad_check_fbacc()
    stopifnot(!missing(name), !missing(campaign_id), !missing(creative_id))

    adgroup_status <- match.arg(adgroup_status)

    ## build params list
    params <- list(
        name           = name,
        campaign_id    = campaign_id,
        creative       = toJSON(list(creative_id = unbox(creative))),
        adgroup_status = adgroup_status)

    ## get results
    res <- fbad_request(
        path   = paste0('act_', fbacc$account_id, '/adgroups'),
        method = "POST",
        params = c(params, list(access_token = fbacc$access_token)))

    ## return campaign ID on success
    fromJSON(res)$id

}
