#' Ping to Binance REST API
#'
#' Check the connection to the Binance API. 
#'
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for endpoint [api/v3/ping](https://binance-docs.github.io/apidocs/spot/en/#test-connectivity). The ip weight is 1.
#'   - `"fapi"`: for endpoint [fapi/v1/ping](https://binance-docs.github.io/apidocs/futures/en/#test-connectivity). The ip weight is 1.
#'   - `"dapi"`: for endpoint [dapi/v1/ping](https://binance-docs.github.io/apidocs/delivery/en/#test-connectivity). The ip weight is 1.
#'   - `"eapi"`: for endpoint [eapi/v1/ping](https://binance-docs.github.io/apidocs/voptions/en/#test-connectivity). The ip weight is 1.
#'
#' @usage 
#' binance_ping(api)
#' 
#' @return A logical value. It is `TRUE` if the connection was successful, otherwise it is `FALSE`.
#'
#' @examples
#' # Test connection to spot api
#' binance_ping("spot")
#'
#' # Test connection to futures usd-m api
#' binance_ping("fapi")
#'
#' # Test connection to futures coin-m api
#' binance_ping("dapi")
#'
#' # Test connection to options api
#' binance_ping("eapi")
#'
#' @keywords rest market ping 
#' @rdname binance_ping
#' @name binance_ping
#' @export

binance_ping <- function(api){
  
  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      msg <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
  }
  
  response <- binance_query(api = api, path = "ping", query = NULL)
  
  if (purrr::is_empty(response)) {
    response <- TRUE
  } else {
    response <- FALSE
  }
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- 1
  
  return(response)
}


