#' Copy Ad Campaign or Ad Set
#' @inheritParams fbad_request
#' @param ad_id (numeric string or integer) id of the ad that you want to copy
#' @param adset_id (numeric string or integer) id adset you want to create a copy of the ad into
#' @param status_option (string) enum {ACTIVE, PAUSED, INHERITED_FROM_SOURCE}
#' @param rename_strategy (string) enum {DEEP_RENAME, ONLY_TOP_LEVEL_RENAME, NO_RENAME}
#' @param rename_prefix (string) a prefix to copy names. Defaults to null if not provided
#' @param rename_suffix (string) a suffix to copy names. Defaults to null if not provided
#' @param ... further arguments passed to the API endpoint
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign/copies/}
fbad_copy_adset <- function(fbacc,
                            ad_id,
                            adset_id = NULL,
                            status_option = NULL,
                            rename_strategy = NULL, 
                            rename_prefix = NULL, 
                            rename_suffix = NULL, ...) {
    
    fbacc <- fbad_check_fbacc()
    
    # ad id missing
    if (missing(ad_id)){
        stop('Argument missing. An ad_id is required.')
    }
    
    # check if ad set id actually exists
    list_of_adsets = fbad_list_adset(fbacc)
    
    if(!adset_id %in% list_of_adsets$id){
        stop('This adset id does not exists. Please provide a valid adset id.')
    }
    
    # rename options
    if(is.null(rename_strategy)){
        rename_options = NULL
    }else if((rename_strategy == "DEEP_RENAME" | rename_strategy == "ONLY_TOP_LEVEL_RENAME") & is.null(rename_prefix) & is.null(rename_suffix)){
        stop("You have selected 'DEEP_RENAME' or 'ONLY_TOP_LEVEL_RENAME' as the argument rename_strategy. You need to specify either the rename_prefix argument, the rename_suffix argument or both")
    }else if(rename_strategy == "NO_RENAME" & !is.null(rename_prefix) || !is.null(rename_suffix)){
        stop("Your rename_stratey is 'NO_RENAME', therefore, you should not select arguments rename_prefix or rename_suffix")
    }else if(!is.null(rename_strategy) & !is.null(rename_prefix)){
        rename_options = list(rename_strategy = rename_strategy,
                              rename_prefix = rename_prefix)
    }else if(!is.null(rename_strategy) & !is.null(rename_suffix)){
        rename_options = list(rename_strategy = rename_strategy,
                              rename_suffix = rename_suffix)
    }else if(is.null(rename_prefix) & is.null(rename_suffix) & !is.null(rename_suffix)){
        rename_options = list(rename_strategy = rename_strategy)
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
                 path   = paste0(adset_id, "/copies?access_token=", fbacc$access_token),
                 method = "POST",
                 params = params)
    
}

# EXAMPLE -----
token = 'xxxxxxxx'
# Define account id - Facebook API > Tests
account_id = "xxxxxx"

library(fbRads)
library(jsonlite)
library(data.table)
library(RCurl)
library(lubridate)
library(futile.logger)

# 3 Initialization ----
# Initialize specific account
fbacc = fbad_init(accountid = account_id, token = token, version = '8.0')

start_time = as_datetime("2020-10-20 23:59:59 PDT")
end_time = as_datetime("2020-10-27 10:10:10 UTC")

campaign_id = "23845893051630648"

fbad_copy_adset(fbacc, 
                adset_id = "23845893193900648",
                campaign_id = campaign_id,
                deep_copy = TRUE,
                status_option = "ACTIVE",
                rename_strategy = "DEEP_RENAME",
                rename_prefix = "Morning-2")

