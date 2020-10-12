#' Copy Ad Campaign or Ad Set
#' @inheritParams fbad_request
#' @param id id of the campaign or adset you want to create a copy of
#' @param ... further arguments passed to the API endpoint
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign/copies/}
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign-group/copies/}
fbad_create_copy <- function(fbacc, id, ...) {
    
    fbacc <- fbad_check_fbacc()
    
    if (missing(id))
        stop('An campaign or adset id is required.')
    
    ## build params list
    params <- list(
        id = id)
    
    ## drop NULL args
    params <- as.list(unlist(params, recursive = FALSE))
    
    ## post request to copy adset
    fbad_request(fbacc,
                 path   = paste0(id, "/copies?access_token=", fbacc$access_token),
                 method = "POST",
                 params = params)
    
}