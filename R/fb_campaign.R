#' Create Ad Campaign
#' @inheritParams fbad_create_ad
#' @return Ad Campaign id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign-group#Creating}
fbad_create_campaign <- fbad_create_ad


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
