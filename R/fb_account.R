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
            business_id = business_id, access_token = access_token,
            name = name, currency = currency, timezone_id = timezone_id,
            end_advertiser = end_advertiser, media_agency = media_agency, partner = partner),
        list(...))

    ## get results
    res <- fbad_request(
        path   = file.path(business_id, 'adaccount'),
        method = "POST",
        params = params)

    ## return campaign ID on success
    fromJSONish(res)$account_id

}


#' Assign a user to an Ad Account
#' @inheritParams fbad_request
#' @param account_id string
#' @param tasks enum
#' @param user id
#' @export
fbad_assign_users_to_account <- function(account_id,access_token,
                                         tasks = c('MANAGE', 'ADVERTISE', 'ANALYZE'),
                                         user) {

    ## fbacc <- fbad_check_fbacc()
    tasks <- match.arg(tasks)

    ## get results
    res <- fbad_request(
        path   = file.path(paste('act', account_id, sep = '_'), 'assigned_users'),
        method = "POST",
        params = list(tasks = tasks, user = user, access_token = access_token))

    ## return campaign ID on success
    invisible(fromJSONish(res)$success)

}
