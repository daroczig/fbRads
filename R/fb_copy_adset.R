#' Create a copy of an ad set. You can copy a maximum of 3 entities between ad set and ads.
#' @inheritParams fbad_request
#' @param adset_id (numeric string or integer) id adset you want to create a copy of
#' @param campaign_id (numeric string or integer) Single ID of a campaign to make parent of the copy. Ignore if you want to keep the copy under the original campaign parent
#' @param start_time (datetime) The start time of the ad set. If not set, the copied adset will inherit the start time from the original set
#' @param end_time (datetime) The end time of the ad set
#' @param deep_copy (boolean) Default value: false. Whether to copy all the child ads
#' @param status_option (string) enum {ACTIVE, PAUSED, INHERITED_FROM_SOURCE}
#' @param rename_strategy (string) enum {DEEP_RENAME, ONLY_TOP_LEVEL_RENAME, NO_RENAME}
#' @param rename_prefix (string) a prefix to copy names. Defaults to null if not provided
#' @param rename_suffix (string) a suffix to copy names. Defaults to null if not provided
#' @param ... further arguments passed to the API endpoint
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign/copies/}
fbad_copy_adset <- function(fbacc,
                             adset_id,
                             campaign_id = NULL,
                             start_time = NULL,
                             end_time = NULL, 
                             deep_copy = NULL,
                             status_option = NULL,
                             rename_strategy = NULL, 
                             rename_prefix = NULL, 
                             rename_suffix = NULL, ...) {
    
    fbacc <- fbad_check_fbacc()
    
    # adset id missing
    if (missing(adset_id)){
        stop('Argument missing. An adset id is required.')
    }
    
    # check if ad set id actually exists
    list_of_adsets = fbad_list_adset(fbacc)
    
    if(!adset_id %in% list_of_adsets$id){
        stop('This adset id does not exists. Please provide a valid adset id.')
    }
    
    # check if campaign id actually exists
    if(!is.null(campaign_id)){
    
        list_of_campaigns = fbad_list_campaign(fbacc)
        
        if(!campaign_id %in% list_of_campaigns$id){
            stop('This campaign id does not exists. Please provide a valid campaign id.')
        }
    
    }
    
    # rename options
    if(is.null(rename_strategy) & !is.null(rename_prefix)){
        stop("You have not selected a rename_strategy, therefore, you should not select arguments rename_prefix")
    }else if(is.null(rename_strategy) & !is.null(rename_suffix)){
        stop("You have not selected a rename_strategy, therefore, you should not select arguments rename_suffix")
    }else if(is.null(rename_strategy) & !is.null(rename_suffix) & !is.null(rename_prefix)){
        stop("You have not selected a rename_strategy, therefore, you should not select arguments rename_suffix or rename_prefix")
    }else if(is.null(rename_strategy) & is.null(rename_prefix) & is.null(rename_suffix)){
        rename_options = NULL
    }else if(rename_strategy == "NO_RENAME" & (!is.null(rename_prefix) | !is.null(rename_suffix))){
        stop("Your rename_stratey is 'NO_RENAME', therefore, you should not select arguments rename_prefix or rename_suffix")
    }else if((rename_strategy == "DEEP_RENAME" | rename_strategy == "ONLY_TOP_LEVEL_RENAME") & is.null(rename_prefix) & is.null(rename_suffix)){
        stop("You have selected 'DEEP_RENAME' or 'ONLY_TOP_LEVEL_RENAME' as the argument rename_strategy. You need to specify either the rename_prefix argument, the rename_suffix argument or both")
    }else if(!is.null(rename_strategy) & !is.null(rename_prefix) & !is.null(rename_suffix)){
        rename_options = list(rename_strategy = rename_strategy,
                              rename_prefix = rename_prefix,
                              rename_suffix = rename_suffix) 
    }else if(!is.null(rename_strategy) & !is.null(rename_prefix)){
        rename_options = list(rename_strategy = rename_strategy,
                              rename_prefix = rename_prefix)
    }else if(!is.null(rename_strategy) & !is.null(rename_suffix)){
        rename_options = list(rename_strategy = rename_strategy,
                              rename_suffix = rename_suffix)
    }else if(!is.null(rename_strategy) & is.null(rename_prefix) & is.null(rename_suffix)){
        rename_options = list(rename_strategy = rename_strategy)
    }
    
    ## build params list
    params <- list(
        campaign_id = campaign_id,
        start_time = start_time,
        end_time = end_time,
        deep_copy = deep_copy,
        status_option = status_option,
        rename_options = rename_options)
    
    ## transform lists to JSON
    params$rename_options <- toJSON(rename_options, auto_unbox = TRUE)
    
    ## drop NULL args
    params <- as.list(unlist(params, recursive = FALSE))
    
    # post request to copy adset
    fbad_request(fbacc,
                 path   = paste0(adset_id, "/copies?access_token=", fbacc$access_token),
                 method = "POST",
                 params = params)
    
}
