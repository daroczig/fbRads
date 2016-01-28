#' Create a new FB custom audience
#' @references \url{https://developers.facebook.com/docs/marketing-api/custom-audience-targeting/v2.4#create}
#' @inheritParams fbad_request
#' @param name string
#' @param description optional string
#' @param opt_out_link optional link
#' @return custom audience ID
#' @export
fbad_create_audience <- function(fbacc, name, description, opt_out_link) {

    fbacc <- fbad_check_fbacc()
    if (missing(name))
        stop('The custom audience name is required.')

    flog.info(paste('Creating new custom audience:', name))

    ## set params
    params <- list(name = name)
    if (!missing(description)) {
        params$description <- description
    }
    if (!missing(opt_out_link)) {
        params$opt_out_link <- opt_out_link
    }

    ## this is a static param required by v2.4
    if (fbacc$api_version >= '2.4') {
        params$subtype <- 'CUSTOM'
    }

    ## get results
    res <- fbad_request(fbacc,
        path   = paste0('act_', fbacc$account_id, '/customaudiences'),
        method = "POST",
        params = params)

    ## return ID
    fromJSON(res)$id

}


#' Read metadata on a FB custom audience
#' @references \url{https://developers.facebook.com/docs/marketing-api/custom-audience-targeting/v2.4#read}
#' @inheritParams fbad_request
#' @param audience_id numeric
#' @param fields character vector of fields to be returned
#' @return custom audience ID
#' @export
fbad_read_audience <- function(fbacc, audience_id, fields = c('id', 'account_id', 'approximate_count','data_source', 'delivery_status', 'lookalike_audience_ids', 'lookalike_spec', 'name', 'permission_for_actions', 'operation_status', 'subtype', 'time_updated')) {

    ## get fields
    fields <- match.arg(fields, several.ok = TRUE)
    fields <- paste(fields, collapse = ',')

    fbacc <- fbad_check_fbacc()
    if (missing(audience_id))
        stop('A custom audience id is required.')

    ## get results
    res <- fbad_request(fbacc,
        path   = paste0(audience_id, '?fields=', fields),
        method = "GET")

    ## return
    fromJSON(res)

}


#' Delete a FB custom audience
#' @references \url{https://developers.facebook.com/docs/marketing-api/custom-audience-targeting/v2.4#delete}
#' @inheritParams fbad_request
#' @param audience_id numeric
#' @return custom audience ID
#' @export
fbad_delete_audience <- function(fbacc, audience_id) {

    fbacc <- fbad_check_fbacc()
    if (missing(audience_id))
        stop('A custom audience id is required.')

    ## get results
    res <- fbad_request(fbacc,
        path   = paste0(audience_id),
        method = "DELETE")

    ## return
    fromJSON(res)

}


#' Share a FB custom audience with other accounts
#' @references \url{https://developers.facebook.com/docs/marketing-api/custom-audience-targeting/v2.3#sharing}
#' @inheritParams fbad_request
#' @param audience_id audience ID
#' @param adaccounts numeric vector of FB account IDs
#' @note This throws error if you provide wrong account ids OR even valid account ids that were previously granted access to the given custom audience.
#' @export
fbad_share_audience <- function(fbacc, audience_id, adaccounts) {

    fbacc <- fbad_check_fbacc()
    flog.info(paste('Sharing', audience_id, 'custom audience ID with', length(adaccounts), 'accounts.'))

    ## make sure adaccounts are integers
    adaccounts <- as.integer64(adaccounts)

    res <- fbad_request(fbacc,
        path   = paste(audience_id, 'adaccounts', sep = '/'),
        method = "POST",
        params = list(adaccounts = toJSON(adaccounts)))

}


#' Add people to a custom FB audience
#' @references \url{https://developers.facebook.com/docs/marketing-api/custom-audience-targeting/v2.4#create}
#' @inheritParams fbad_request
#' @param audience_id string
#' @param schema only two schema are supported out of the four: you can add/remove persons to/from a custom audience by e-mail addresses or phone numbers
#' @param hashes character vector of e-mail addresses or phone numbers to be transformed to hashes
#' @export
fbad_add_audience <- function(fbacc, audience_id,
                              schema = c('EMAIL', 'PHONE'),
                              hashes) {

    ## match called function name for future reference
    fn <- deparse(match.call()[[1]])

    ## check params and log
    fbacc <- fbad_check_fbacc()
    flog.info(paste(switch(fn, 'fbad_add_audience' = 'Adding', 'Removing'),
                    length(hashes), schema,
                    switch(fn, 'fbad_add_audience' = 'to', 'from'),
                    audience_id, 'custom audience ID.'))

    if (length(hashes) == 0) {

        warning('Nothing to send to FB')

    } else {

        ## compute hashes for e-mail or phone numbers
        hashes <- sapply(hashes, digest, serialize = FALSE,
                         algo = 'sha256', USE.NAMES = FALSE)

        ## split hashes into 10K groups when adding and 1K when deleting
        hashes <- split(hashes, 1:length(hashes) %/%
                                switch(fn, 'fbad_add_audience' = 1e4, 1e3))

        ## get results
        sapply(hashes, function(hash)
            fbad_request(fbacc,
                path   = paste(audience_id, 'users', sep = '/'),
                method = switch(fn, 'fbad_add_audience' = 'POST', 'DELETE'),
                params = list(
                    payload      = toJSON(c(
                        list(schema = unbox(paste0(schema, '_SHA256'))),
                        list(data   = hash))))))

    }

    ## TODO parse results and error handling

}


#' Add people from a custom FB audience
#' @inheritParams fbad_add_audience
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/custom-audience/users#Deleting}
fbad_remove_audience <- fbad_add_audience


#' Create a new FB lookalike audience similar to an already existing custom audience
#' @references \url{https://developers.facebook.com/docs/marketing-api/lookalike-audience-targeting/v2.4#create}
#' @inheritParams fbad_request
#' @param name string
#' @param origin_audience_id numeric ID of origin custom audience
#' @param ratio Between 0.01-0.20 and increments of 0.01. Indicates the top \code{ratio} percent of original audience in the selected country
#' @param country Country name - the country to find the lookalike people.
#' @return lookalike audience ID
#' @export
fbad_create_lookalike_audience <- function(fbacc, name, origin_audience_id, ratio = 0.01, country = 'US') {

    fbacc <- fbad_check_fbacc()
    if (missing(name))
        stop('A custom name for the lookalike audience is required.')
    if (missing(origin_audience_id))
        stop('The origin custom audience id is required.')

    flog.info(paste0('Creating new lookalike (', ratio*100, '%%) ', country, ' audience based on ', origin_audience_id, ': ', name))

    ## set params
    params <- list(
        name               = name,
        origin_audience_id = origin_audience_id,
        lookalike_spec     = toJSON(list(
            ratio   = ratio,
            country = country
            ), auto_unbox = TRUE))

    ## this is a static param required by v2.4
    if (fbacc$api_version >= '2.4') {
        params$subtype <- 'LOOKALIKE'
    }

    ## get results
    res <- fbad_request(fbacc,
        path   = paste0('act_', fbacc$account_id, '/customaudiences'),
        method = "POST",
        params = params)

    ## return ID
    fromJSON(res)$id

}
