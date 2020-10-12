#' Copy Ad Campaign or Ad Set
#' @inheritParams fbad_request
#' @param id (character) id of the campaign or adset you want to create a copy of
#' @param end_time (datetime) The end time of the ad set
#' @param deep_copy (boolean) Default value: false. Whether to copy all the child ads.
#' @param rename_strategy (string) enum {DEEP_RENAME, ONLY_TOP_LEVEL_RENAME, NO_RENAME}
#' @param rename_prefix (string) a prefix to copy names. Defaults to null if not provided.
#' @param rename_suffix (string) a suffix to copy names. Defaults to null if not provided.
#' @param ... further arguments passed to the API endpoint
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign/copies/}
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign-group/copies/}
fbad_create_copy <- function(fbacc, 
                             id, 
                             end_time = NULL, 
                             deep_copy = NULL, 
                             rename_strategy = NULL, 
                             rename_prefix = NULL, 
                             rename_suffix = NULL, ...) {
    
    fbacc <- fbad_check_fbacc()
    
    # id
    if (missing(id)){
        stop('An campaign or adset id is required.')
    }
    
    # rename options
    if(!is.null(rename_strategy) && !is.null(rename_prefix)){
        rename_options = list(rename_strategy = rename_strategy,
                              rename_prefix = rename_prefix)
    }else if(!is.null(rename_strategy) && !is.null(rename_suffix)){
        rename_options = list(rename_strategy = rename_strategy,
                              rename_suffix = rename_suffix)
    }else if(is.null(rename_prefix) && is.null(rename_suffix) && !is.null(rename_suffix)){
        rename_options = list(rename_strategy = rename_strategy)
    }else{
        rename_options = NULL
    }
    
    ## build params list
    params <- list(
        id = id,
        end_time = end_time,
        deep_copy = deep_copy,
        rename_options = rename_options)
    
    ## transform lists to JSON
    params$rename_options <- toJSON(rename_options, auto_unbox = TRUE)
    
    ## drop NULL args
    params <- as.list(unlist(params, recursive = FALSE))
    
    ## post request to copy adset
    fbad_request(fbacc,
                 path   = paste0(id, "/copies?access_token=", fbacc$access_token),
                 method = "POST",
                 params = params)
    
}

token = 'EAADhONh9vckBAFUT4VE468jfQKg7g9YGKgF25ZBASkhzdAU9H6BhNq1Dea5xZAcQrzVIv8kwzZCvpfu2Y9SE7G6psPUKfQ0LtCUZAtWUeSofzuOpeAXKRyRuDAnlS35WW8AeZB8LKZBorZADZAxUwXjBb9OBKleiWj32ZBDVOz7n2qzXZAsZBZA1rJbi3w7PR19JGK8ZD'
# Define account id - Facebook API > Tests
account_id = "347374566526931"

library(fbRads)
library(jsonlite)
library(data.table)
library(RCurl)
library(lubridate)

# 3 Initialization ----
# Initialize specific account
fbacc = fbad_init(accountid = account_id, token = token, version = '8.0')

fbad_create_copy(fbacc, id = "23845893193900648", 
                 end_time = end_time, 
                 deep_copy = TRUE)

