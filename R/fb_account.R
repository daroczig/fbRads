#' Create Ad Account
#' @param business_id Business Manager id
#' @param access_token token
#' @param name string
#' @param currency ISO 4217 Currency Code
#' @param timezone_id \url{https://developers.facebook.com/docs/marketing-api/reference/ad-account/timezone-ids}
#' @param end_advertiser string
#' @param media_agency string
#' @param partner string
#' @param ... further parameters passed to the Facebook API
#' @return Ad Account id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-account#Creating}
fbad_create_account <- function(business_id, access_token,
                                name, currency, timezone_id,
                                end_advertiser = 'NONE', media_agency = 'NONE',
                                partner = 'NONE', ...) {

    ## build params list
    params <- c(
        list(
            business_id = business_id, access_token = access_token,cccount_id

}
