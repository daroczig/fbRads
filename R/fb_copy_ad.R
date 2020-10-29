#' Create a copy of an ad.
#' @inheritParams fbad_request
#' @param ad_id (numeric string or integer) id of the ad that you want to copy
#' @param adset_id (numeric string or integer) id adset you want to create a copy of the ad into
#' @param status_option (string) enum {ACTIVE, PAUSED, INHERITED_FROM_SOURCE}
#' @param rename_strategy (string) enum {DEEP_RENAME, ONLY_TOP_LEVEL_RENAME, NO_RENAME}
#' @param rename_prefix (string) a prefix to copy names. Defaults to null if not provided
#' @param rename_suffix (string) a suffix to copy names. Defaults to null if not provided
#' @param ... further arguments passed to the API endpoint
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/adgroup/copies#Creating}
fbad_copy_ad <- function(fbacc,
                         ad_id,
                         adset_id = NULL,
                         status_option = NULL,
                         rename_strategy = NULL,
                         rename_prefix = NULL,
                         rename_suffix = NULL, ...) {

    fbacc <- fbad_check_fbacc()

    # ad id missing
    if (missing(ad_id)) {
        stop('Argument missing. An ad id is required.')
    }

    # check if ad id actually exists
    list_of_ads <- fbad_list_ad(fbacc)

    if(!ad_id %in% list_of_ads$id) {
        stop('This ad id does not exists. Please provide a valid ad id.')
    }

    # check if adset id actually exists
    if(!is.null(adset_id)) {

        list_of_adsets <- fbad_list_adset(fbacc)

        if(!adset_id %in% list_of_adsets$id) {
            stop('This ad set id does not exists. Please provide a valid ad set id.')
        }

    }

    # rename options
    if(is.null(rename_strategy) & !is.null(rename_prefix)) {
        stop("You have not selected a rename_strategy, therefore, you should not select arguments rename_prefix")
    } else if(is.null(rename_strategy) & !is.null(rename_suffix)) {
        stop("You have not selected a rename_strategy, therefore, you should not select arguments rename_suffix")
    } else if(is.null(rename_strategy) & !is.null(rename_suffix) & !is.null(rename_prefix)) {
        stop("You have not selected a rename_strategy, therefore, you should not select arguments rename_suffix or rename_prefix")
    } else if(is.null(rename_strategy) & is.null(rename_prefix) & is.null(rename_suffix)) {
        rename_options <- NULL
    } else if(rename_strategy == "NO_RENAME" & (!is.null(rename_prefix) | !is.null(rename_suffix))) {
        stop("Your rename_stratey is 'NO_RENAME', therefore, you should not select arguments rename_prefix or rename_suffix")
    } else if((rename_strategy == "DEEP_RENAME" | rename_strategy == "ONLY_TOP_LEVEL_RENAME") & is.null(rename_prefix) & is.null(rename_suffix)) {
        stop("You have selected 'DEEP_RENAME' or 'ONLY_TOP_LEVEL_RENAME' as the argument rename_strategy. You need to specify either the rename_prefix argument, the rename_suffix argument or both")
    } else if(!is.null(rename_strategy) & !is.null(rename_prefix) & !is.null(rename_suffix)) {
        rename_options <- list(rename_strategy = rename_strategy,
                               rename_prefix = rename_prefix,
                               rename_suffix = rename_suffix)
    } else if(!is.null(rename_strategy) & !is.null(rename_prefix)) {
        rename_options <- list(rename_strategy = rename_strategy,
                               rename_prefix = rename_prefix)
    } else if(!is.null(rename_strategy) & !is.null(rename_suffix)) {
        rename_options <- list(rename_strategy = rename_strategy,
                               rename_suffix = rename_suffix)
    } else if(!is.null(rename_strategy) & is.null(rename_prefix) & is.null(rename_suffix)) {
        rename_options <- list(rename_strategy = rename_strategy)
    }

    ## build params list
    params <- list(
        adset_id = adset_id,
        status_option = status_option,
        rename_options = rename_options)

    ## transform lists to JSON
    params$rename_options <- toJSON(rename_options, auto_unbox = TRUE)

    ## drop NULL args
    params <- as.list(unlist(params, recursive = FALSE))

    # post request to copy adset
    fbad_request(fbacc,
                 path   = paste0(ad_id, "/copies?access_token=", fbacc$access_token),
                 method = "POST",
                 params = params)

}
