#' Create Ad Set
#' @inheritParams fbad_create_ad
#' @return Ad Set id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#Creating}
fbad_create_adset <- fbad_create_ad


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
