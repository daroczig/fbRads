#' Copy Ad Campaign or Ad Set
#' @inheritParams fbad_request
#' @param id (character) id of the campaign or adset you want to create a copy of
#' @param end_time (datetime) The end time of the ad set
#' @param deep_copy (boolean) Default value: false. Whether to copy all the child ads.
#' @param ... further arguments passed to the API endpoint
#' @export
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign/copies/}
#' @references \url{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign-group/copies/}
fbad_create_copy <- function(fbacc, id, end_time, deep_copy, ...) {
    
    fbacc <- fbad_check_fbacc()
    
    if (missing(id))
        stop('An campaign or adset id is required.')
    
    ## build params list
    params <- list(
        id = id,
        end_time = end_time,
        deep_copy = deep_copy)
    
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

end_time = as_datetime("2020-10-25 23:59:59 PDT") 

fbad_create_copy(fbacc, id = "23845893193900648", end_time = end_time, deep_copy = TRUE)

?fbad_init
