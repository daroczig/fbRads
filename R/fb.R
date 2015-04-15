## we build the functions to support one given version of the API
## so the users should not be able to override that
fbad_api_version <- 2.2


#' Get versioned base url
#' @param version the version for which a base url is being generated
#' @return character URL with trailing slash
fbad_get_baseurl <- function() {
  paste('https://graph.facebook.com', fb_api_version, '', sep = '/')
}


#' Get results of a synchronous query from FB graph API
#' @param base_url versioned ads API base url
#' @param request_path API request path (i.e. endpoint)
#' @param style HTTP request type (e.g. GET, POST, DELETE)
#' @param params a name-value list of form parameters for API query
#' @param ... RCurl options to pass along to HTTP request
#' @return json object containing results
fb_query_api <- function(base_url,
                         request_path,
                         style,
                         params,
                         ... ){
  require(RCurl)

  ## Check that params meet certain standards
  params <- fb_check_curl_params(params)

  ## Generate query based on HTTP method
  if(style == "GET") { # Use getForm
    ff <- "getForm"
  } else if(style %in% c("POST","HTTPPOST")) { # Use postForm
    ff <- "postForm"
  } else {
    stop("Unsupported HTTP method, check the function docs!")
  }

  ## Parse and eval
  resp <- eval(
    parse( text = paste(ff ,
                        "( uri = paste0(base_url,request_path),",
                        ".params = params,",
                        "... ,",
                        "style = style )"
    )
    )
  )
  ## Full json output
  resp
}


#' Run basic checks on curl get/post parameters
#' @param params named list of parameters to GET/POST
#' @return list if OK, error if not
fb_check_curl_params <-function(param_list){

  ## Length check
  if(length(param_list) == 0){
    stop("No parameters to get or set, pls debug this code")
  }

  ## Names check
  if( any( is.null( names(param_list) ) ) ){
    stop("Parameters missing field names, expecting 'field = value'")
  } else if( ! "access_token" %in% names(param_list) ){
    stop("access_token missing from cURL request")
  }

  ## Values check (not required)
  if(any( as.vector(unlist(param_list)) %in% c(NA, NULL))){
    stop("NA or NULL values not allowed in cURL request")
  }

  ## Return params if all OK
  param_list

}



#' Initiate Facebook Account and Token
#' @param accountid Facebook Ad account id to
#' @param version Facebook Marketing API version to use
#' @param rdstoken Location of the serialized FB Ads API access token
#' @return list containing versioned base URL and relevant API parameters
#' @export
fb_init_adaccount <- function(accountid,
                              rdstoken="data/reference/foxtoken20150301.rds") {
  ## FB Graph API base url
  versioned_url <- fb_get_baseurl()

  ## Create base_params
  token <- readRDS(rdstoken)
  output_params <- list(
    "acct_path"= paste0("act_",accountid,"/"),
    "versioned_url" = versioned_url ,
    "access_token" = token$token,
    "api_version" = fbad_api_version
  )

  #   ## Get Ad Accounts
  #   accessible <- fb_get_adaccounts(output_params$access_token)
  #   # If token has account access (diable if API misbehaves)
  #
  #   if(! accountid %in% accessible){
  #     stop("Auth token does not have rights for account requested")
  #   }

  ## Account details
  details <- fb_get_adaccount_details(accountid, output_params$access_token)
  print(paste0("Account (", details$name,
               ") initiated with token (",
               token$username,")")
  )

  ## Output fb_env
  #   fb_env <<- c(output_params, details)
  c(output_params, details)

}

#' FB Search API Querying
#' https://developers.facebook.com/docs/marketing-api/targeting-search/v2.2
#' @param q string that is being searched for
#' @param type describes the type of search eg: adinterest, adeducationmajor etc
#' @param ... other optional parameters accepted by the endpoint as key = value
#' pairs eg: limit = 5000.
#' @return data frame containing results
fb_get_targetsearch <- function( env = fb_env, term, searchtype, ... ){
  require(plyr)
  require(RCurl)
  require(jsonlite)
  params <- list(...)
  defaults <- list("access_token" = env$access_token,
                   "limit" = 500,
                   "type" = searchtype,
                   "list" = "GLOBAL"
  )
  ## handle parameter variability
  if(length(params) > 0){
    prms <- c(params, defaults)
  } else {
    prms <- defaults
  }

  ## Handle term input variation in API
  if(searchtype %in% c("adinterestvalid",
                       "adinterestsuggestion")

  ){
    prms <- c(prms, list("interest_list" = toJSON((term))))
  } else {
    if(length(q) > 1){
      warning("Multiple keywords not allowed. Using first")
      q <- q[1]
    } else {
      prms <- c(prms, list("q" = as.character(term)))
    }
  }

  properties <- fb_query_api(base_url = fb_env$versioned_url,
                             request_path= "search",
                             style = "GET",
                             params = prms
  )

  if(searchtype %in% c("adinterestvalid",
                       "adinterestsuggestion","adinterest")

  ){
    if(class(fromJSON(properties)$data) == "data.frame"){
      ans <- fromJSON(properties)$data
    } else if (class(fromJSON(properties)$data) == "list"){
      ans  <- ldply(fromJSON(properties)$data,
                    function(x) data.frame(id = x$id,
                                           name = x$name,
                                           audience_size = x$audience_size
                    )
      )
    }
    ans

  } else{
    ldply(fromJSON(properties)$data, as.data.frame)
  }
}
