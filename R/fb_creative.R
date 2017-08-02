#' Create an ad creative
#' @inheritParams fbad_request
#' @param title string
#' @param body string
#' @param name string
#' @param actor_id Facebook object ID reference
#' @param follow_redirect boolean
#' @param image_file local image passed to Facebook. You might first upload the image via \code{fbad_create_image}.
#' @param image_hash string
#' @param image_crops list
#' @param image_url string
#' @param link_url string
#' @param url_tags list
#' @param object_id Facebook object ID reference
#' @param object_story_id post ID reference
#' @param object_story_spec list
#' @param object_url string list
#' @param call_to_action_type string
#' @return creative id
#' @export
#' @note There are no checks done before passing provided arguments to Facebook. You have to know what you are up to. Read the Facebook docs.
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-creative#Creating}
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
    fromJSONish(res)$id

}


#' Read ad creative
#' @inheritParams fbad_request
#' @param id Ad Creative or Ad Set id
#' @param by get all Ad Creatives for the account, for a given Ad or a single Creative
#' @param fields character vector of fields to get from the API
#' @return list to be post-processed
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-creative#Reading}
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
    fromJSONish(res)

}
