#' @name Api
#' @rdname Api
#' @description Generic function to execute an API GET call.
#' @param url The base url for the Api, usually can be found at the beginning of the documentation.
#' @param path a vector containing the Api path.
#' @param query a named list containing the query parameters.
#' @param config list
#' @param type "text"
#' @param enconding "UTF-8"
#' @param json TRUE
#' @param handle NULL
#' @param quiet TRUE
#' @param ... see
#' @return Returns an `list`.
#' @export


Api <- function(url = NULL, path = NULL, query = NULL, config = list(), type = "text", encoding = "UTF-8", json = TRUE, handle = NULL, quiet = FALSE, ...){

  # Initialize the result
  api_content <- NULL

  # Api Path: remove null/NA elements
  api_path <- path[!is.null(path) && !is.na(path)]

  # Api Query: remove null/NA elements
  api_query <- query[!is.null(query) && !is.na(query)]

  # Api url
  api_url <- httr::modify_url(url, path = api_path, query = api_query)

  # Api GET call
  result <- httr::GET(api_url, config = config,..., handle = handle )

  # Check status and result
  api_status <- httr::status_code(result)
  api_empty  <- purrr::is_empty(result)

  # Error 1: status code is not equal to 200
  if(api_status != 200){
    msg <- httr::content(result)$msg
    warn <- paste0("GET Request ERROR: the status code is not equal to 200.", "\n \n",
                   "Error Message: ", msg)
    if(!quiet) warning(warn)
    return(api_content)
    }

  # Error 2: the result is empty
  if(api_empty){
    if(!quiet) warning("GET Request Error: the response is empty!")
    return(api_content)
    }

  # Get the Api content
  if(api_status == 200 && !api_empty){

    api_content <- httr::content(result, type = type, encoding = encoding)

    # if json = TRUE, conversion from JSON into an R list
    if(json){
      api_content <- jsonlite::fromJSON(api_content)
    }
  }

  return(api_content)
}
