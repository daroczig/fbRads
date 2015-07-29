#' Create an ad creative
#' @inheritParams fbad_request
#' @param title
#' @param body
#' @param name
#' @param actor_id
#' @param follow_redirect
#' @param image_file
#' @param image_hash
#' @param image_crops
#' @param image_url
#' @param link_url
#' @param url_tags
#' @param object_id
#' @param object_story_id
#' @param object_story_spec
#' @param object_url
#' @param call_to_action_type
#' @return creative id
#' @export
#' @note There are no checks done before passing provided arguments to Facebook. You have to know what you are up to. Read the Facebook docs.
#' @references \url{https://developers.facebook.com/docs/marketing-api/adcreative/v2.4#create}
fbad_create_creative <- function(
    fbacc,
    title, body, name,
    actor_id, follow_redirect,
    image_file, image_hash, image_crops, image_url,
    link_url, url_tags,
    object_id, object_story_id, object_story_spec, object_url,
    call_to_action_type = c(
        'OPEN_LINK', 'BOOK_TRAVEL', 'SHOP_NOW',
        'PLAY_GAME', 'LISTEN_MUSIC', 'WATCH_VIDEO', 'USE_APP')) {

    fbacc <- fbad_check_fbacc()
    if (!missing(call_to_action_type)) {
        call_to_action_type <- match.arg(call_to_action_type)
    }

    ## get arguments
    mc <- match.call()
    mc[[1]] <- quote(list)

    ## drop fbacc from the arguments
    mc$fbacc <- NULL

    ## eval in parent.frame to get the parameter values
    params <- eval(mc, envir = parent.frame())

    ## get results
    res <- fbad_request(fbacc,
        path   = paste0('act_', fbacc$account_id, '/adcreatives'),
        method = "POST",
        params = params)

    ## return
    fromJSON(res)$id

}


#' Read ad creative
#' @inheritParams fbad_request
#' @param creative
#' @param fields
#' @return list
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/adcampaign/v2.4#read}
#' @examples \dontrun{
#' ## get all creatives for the current account
#' fbad_read_creative(fbacc)
#' }
fbad_read_creative <- function(
    fbacc, id,
    by = c('account', 'creative', 'ad'),
    fields = c(
        'id', 'name', 'body', 'title', 'run_status',
        'actor_id', 'call_to_action_type', 'follow_redirect',
        'image_crops', 'image_file', 'image_hash', 'image_url', 'thumbnail_url',
        'link_url', 'url_tags',
        'object_id', 'object_story_id', 'object_story_spec', 'object_type', 'object_url')) {

    fbacc <- fbad_check_fbacc()

    by <- match.arg(by)
    ## default to current account id
    if (by == 'account' && missing(id)) {
        id <- fbacc$account_id
    }

    ## get fields
    fields <- match.arg(fields, several.ok = TRUE)
    fields <- paste(fields, collapse = ',')

    ## we need an id
    if (by != 'account' && missing(id))
        stop('An id is required.')

    ## get results
    res <- fbad_request(fbacc,
        path   = switch(by,
            'creative' = paste0(id, '?fields=', fields),
            'account'  = paste0('act_', id, '/adcreatives?fields=', fields),
            'ad'       = paste0(id, '/adcreatives?fields=', fields)),
        method = "GET")

    ## return
    fromJSON(res)

}
