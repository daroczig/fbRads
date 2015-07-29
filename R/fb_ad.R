#' Create ad
#' @param fbacc (optional) \code{FB_Ad_account} object, which defaults to the last returned object of \code{\link{fbad_init}}.
#' @param name
#' @param campaign_id
#' @param creative_id
#' @param adgroup_status
#' @param ... further parameters passed to the Facebook API
#' @return ad id
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/adgroup/v2.4#create}
fbad_create_ad <- function(fbacc,
                                 name, campaign_id, creative_id,
                                 adgroup_status = c('ACTIVE', 'PAUSED'), ...) {

    fbacc <- fbad_check_fbacc()
    stopifnot(!missing(name), !missing(campaign_id), !missing(creative_id))

    adgroup_status <- match.arg(adgroup_status)

    ## build params list
    params <- list(
        name           = name,
        campaign_id    = campaign_id,
        creative       = toJSON(list(creative_id = unbox(creative))),
        adgroup_status = adgroup_status)

    ## add further params if provided
    if (length(list(...)) > 0) {
        params <- c(params, list(...))
    }

    ## get results
    res <- fbad_request(fbacc,
        path   = paste0('act_', fbacc$account_id, '/adgroups'),
        method = "POST",
        params = params)

    ## return campaign ID on success
    fromJSON(res)$id

}


#' Read ad
#' @param fbacc (optional) \code{FB_Ad_account} object, which defaults to the last returned object of \code{\link{fbad_init}}.
#' @param id ad id(s)
#' @param fields character vector of fields to get from the API -- please refer to the FB documentation for a list of possible values.
#' @return data.frame
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/adgroup/v2.4#read}
fbad_read_ad <- function(fbacc, id, fields = 'id') {

    fbacc <- fbad_check_fbacc()

    ## merge fields
    fields <- paste(fields, collapse = ',')

    ## we need at least one ad id
    if (missing(id)) {
        stop('Please provide at least one ad id.')
    }

    ## get results for only one id
    if (length(id) == 1) {
        res <- fbad_request(fbacc,
                            path   = id,
                            params = list(fields = fields),
                            method = "GET")
        return(as.data.frame(fromJSON(res)))
    }

    ## or do batched query
    res <- lapply(
        split(id, 1:length(id) %/% 50),
        function(batch) {

            ## query FB by 50 ids at a time
            res <- fbad_request(fbacc,
                                path   = '',
                                params = list(
                                    ids    = paste(batch, collapse = ','),
                                    fields = fields),
                                method = "GET")

            ## transform to data.frame
            do.call(rbind, lapply(fromJSON(res),
                                  as.data.frame, stringsAsFactors = FALSE))

        })

    ## return
    do.call(rbind, res)

}


#' Update ad
#' @param fbacc (optional) \code{FB_Ad_account} object, which defaults to the last returned object of \code{\link{fbad_init}}.
#' @param id ad id
#' @param ... parameters passed to Facebook API
#' @return invisible TRUE
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/adgroup/v2.4#update}
fbad_update_ad <- function(fbacc, id, ...) {

    fbacc <- fbad_check_fbacc()

    ## we need one and only one id
    if (missing(id)) {
        stop('Please provide at least one ad id.')
    }

    ## query Facebook API
    res <- fbad_request(fbacc,
                        path   = id,
                        params = list(...),
                        method = "POST")

    ## success
    invisible(fromJSON(res)$success)

}



