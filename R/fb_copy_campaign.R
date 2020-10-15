#' Copy Ad Campaign or Ad Set
#' @inheritParams fbad_request
#' @param campaign_id (numeric string or integer) Single ID of a campaign to copy
#' @param start_time (datetime) The start time of the ad set. If not set, the copied adset will inherit the start time from the original set
#' @param end_time (datetime) The end time of the ad set
#' @param deep_copy (boolean) Default value: false. Whether to copy all the child ads
#' @param status_option (string) enum {ACTIVE, PAUSED, INHERITED_FROM_SOURCE}
#' @param rename_strategy (string) enum {DEEP_RENAME, ONLY_TOP_LEVEL_RENAME, NO_RENAME}
#' @param rename_prefix (string) a prefix to copy names. Defaults to null if not provided
#' @param rename_suffix (string) a suffix to copy names. Defaults to null if not provided
#' @param ... further arguments passed to the API endpoint
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign-group/copies/}
fbad_copy_campaign <- function(fbacc,
                            campaign_id,
                            start_time = NULL,
                            end_time = NULL, 
                            deep_copy = NULL,
                            status_option = NULL,
                            rename_strategy = NULL, 
                            rename_prefix = NULL, 
                            rename_suffix = NULL, ...) {
    
    fbacc <- fbad_check_fbacc()
    
    # campaign id missing
    if (missing(campaign_id)){
        stop('Argument missing. A campaign id is required.')
    }
    
    # check if campaign id actually exists
    list_of_campaigns = fbad_list_campaign(fbacc)
    
    if(!campaign_id %in% list_of_campaigns$id){
        stop('This campaign id does not exists. Please provide a valid campaign id.')
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
         path   = paste0(campaign_id, "/copies?access_token=", fbacc$access_token),
         method = "POST",
         params = params)
    
}

# EXAMPLE -----
token = 'EAADhONh9vckBAFUT4VE468jfQKg7g9YGKgF25ZBASkhzdAU9H6BhNq1Dea5xZAcQrzVIv8kwzZCvpfu2Y9SE7G6psPUKfQ0LtCUZAtWUeSofzuOpeAXKRyRuDAnlS35WW8AeZB8LKZBorZADZAxUwXjBb9OBKleiWj32ZBDVOz7n2qzXZAsZBZA1rJbi3w7PR19JGK8ZD'
# Define account id - Facebook API > Tests
account_id = "347374566526931"

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

campaign_id = "xxxxxxxxxx"

fbad_copy_campaign(fbacc, 
                campaign_id = "23845893051630648",
                deep_copy = FALSE,
                status_option = "ACTIVE",
                rename_strategy = "DEEP_RENAME",
                rename_prefix = "Morning-2")

