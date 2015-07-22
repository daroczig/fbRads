#' Upload image
#' @param fbacc (optional) \code{FB_Ad_account} object, which defaults to the last returned object of \code{\link{fbad_init}}.
#' @param img
#' @return list of file name, hash and URL
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/adimage/v2.3#create}
fbad_create_image <- function(fbacc, img) {

    fbacc <- fbad_check_fbacc()

    ## we need an img file
    if (missing(img) || !file.exists(img)) {
        stop('A valid image file path is required.')
    }

    ## get results
    res <- fbad_request(fbacc,
        path   = paste0('act_', fbacc$account_id, '/adimages'),
        method = "POST",
        params = list(image_file = fileUpload(img)))

    ## parse JSON
    res <- fromJSON(res)[[1]]

    ## return
    list(filename = names(res), hash = res[[1]]$hash, url = res[[1]]$url)

}
