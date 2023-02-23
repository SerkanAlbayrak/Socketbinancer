#' @name binance_
#' @rdname binance_
#' @description Generic function to execute an Binance API call.
#' @param .fun function
#' @param api type of api, can be "SPOT", "COIN-M", "USD-M" or "OPTIONS"
#' @param vision FALSE
#' @param ... see
#' @return Returns an `tibble`.
#' @examples
#'
#' binance_(.fun = "ping", api = "SPOT")
#' binance_(.fun = "ping", api = "COIN-M")
#' binance_(.fun = "ping", api = "USD-M")
#' binance_(.fun = "ping", api = "OPTIONS")
#'
#' binance_(.fun = "server", api = "SPOT")
#' binance_(.fun = "server", api = "COIN-M")
#' binance_(.fun = "server", api = "USD-M")
#' binance_(.fun = "server", api = "OPTIONS")
#'
#' binance_(.fun = "exchangeInfo", api = "SPOT")
#' binance_(.fun = "exchangeInfo", api = "COIN-M")
#' binance_(.fun = "exchangeInfo", api = "USD-M")
#' binance_(.fun = "exchangeInfo", api = "OPTIONS")
#'
#' binance_(.fun = "lastTrades", api = "SPOT", pair = "ETHUSDT")
#' binance_(.fun = "lastTrades", api = "COIN-M", pair = "ETHUSD_PERP")
#' binance_(.fun = "lastTrades", api = "USD-M", pair = "ETHUSDT")
#' binance_(.fun = "lastTrades", api = "OPTIONS", pair = "BTC-230224-24000-P")
#'
#' binance_(.fun = "depth", api = "SPOT", pair = "ETHUSDT")
#' binance_(.fun = "depth", api = "COIN-M", pair = "ETHUSD_PERP")
#' binance_(.fun = "depth", api = "USD-M", pair = "ETHUSDT")
#' binance_(.fun = "depth", api = "OPTIONS", pair = "BTC-230224-24000-P")
#'
#' binance_(.fun = "aggTrades", api = "SPOT", pair = "ETHUSDT", from = NULL, to = NULL)
#' binance_(.fun = "aggTrades", api = "COIN-M", pair = "ETHUSD_PERP", from = NULL, to = NULL)
#' binance_(.fun = "aggTrades", api = "USD-M", pair = "ETHUSDT", from = NULL, to = NULL)
#'
#' binance_(.fun = "kline", api = "SPOT", pair = "ETHUSDT", from = NULL, to = NULL, interval = "1m")
#' binance_(.fun = "kline", api = "COIN-M", pair = "ETHUSD_PERP", from = NULL, to = NULL)
#' binance_(.fun = "kline", api = "USD-M", pair = "ETHUSDT", from = NULL, to = NULL)
#' binance_(.fun = "kline", api = "OPTIONS", pair = "BTC-230224-24000-P", from = NULL, to = NULL)
#'
#' binance_(.fun = "ticker24h", api = "SPOT", pair = "ETHUSDT", type = "full")
#' binance_(.fun = "ticker24h", api = "COIN-M", pair = "ETHUSD_PERP")
#' binance_(.fun = "ticker24h", api = "USD-M", pair = "ETHUSDT")
#' binance_(.fun = "ticker24h", api = "OPTIONS", pair = "BTC-230224-24000-P")
#'
#' @export

binance_ <- function(.fun = "ping", api = "SPOT", vision = FALSE, ...){

  # Choose an Api
  api <- toupper(api)

  if(vision){

    api <- match.arg(api, choices = c(spot = "SPOT", futures = "COIN-M", futures = "USD-M"))

    name <- match.arg(.fun, choices = c("trade", "aggTrades", "kline"))

    # Choose a Function
    .fun <- paste0("binance_vision_", names(api), "_", name)

  } else {

    api <- match.arg(api, choices = c(api = "SPOT", dapi = "COIN-M", fapi = "USD-M", eapi = "OPTIONS"))

    # Choose a Function
    if(api == "SPOT"){
      name <- match.arg(.fun, choices = c("ping", "server", "exchangeInfo", "lastTrades",
                                          "depth", "aggTrades", "kline", "ticker24h"))
    } else if(api %in% c("COIN-M", "USD-M")){
      name <- match.arg(.fun, choices = c("ping", "server", "exchangeInfo", "lastTrades", "depth",
                                          "aggTrades", "kline", "ticker24h", "openInterest", "openInterestHist",
                                          "topLongShortAccountRatio", "topLongShortPositionRatio", "globalLongShortAccountRatio",
                                          "takerBuySellVol"))
    } else {
      name <- match.arg(.fun, choices = c("ping", "server", "exchangeInfo", "lastTrades",
                                          "depth", "kline", "ticker24h", "openInterest"))
    }

    # Function
    .fun <- paste0("binance_", names(api), "_", name)
  }

  # Arguments
  .args <- list(...)

  # Output
  response <- do.call(what = .fun, args = .args)

  attr(response, "api")  <-  api
  attr(response, "fun")  <-  name
  attr(response, "args") <- .args
  attr(response, "vision") <- vision

  return(response)

}

# --------------------------------         ping        ------------------------------------------------------
#' @name binance_api_ping
#' @rdname binance_api_ping
#' @description Test connectivity to the Binance's Rest API.
#' @return Returns a Boolean object, it is TRUE if the connection is successful, otherwise FALSE
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#' @examples
#' # Test the connection
#' binance_api_ping()
#' binance_fapi_ping()
#' binance_dapi_ping()
#' binance_eapi_ping()
#' @export

binance_api_ping <- function(){

  url <- "https://api.binance.com"

  response <- FALSE

  api_path <- c("api", "v3", "ping" )

  ping <- Api(url = url, path = api_path)

  response <- ifelse(purrr::is_empty(ping), TRUE, FALSE)

  attr(response, "IpWgt") <- 1
  attr(response, "api") <- "SPOT"
  return(response)
}

#'@name binance_fapi_ping
#'@rdname binance_fapi_ping
#'@description see
#'
#' @examples
#' # Test the connection
#' binance_fapi_ping()
#'
#' @export

binance_fapi_ping <- function(){

  url <- "https://fapi.binance.com"

  response <- FALSE

  api_path <- c("fapi", "v1", "ping" )

  ping <- Api(url = url, path = api_path)

  response <- ifelse(purrr::is_empty(ping), TRUE, FALSE)

  attr(response, "IpWgt") <- 1
  attr(response, "api") <- "USD-M"
  return(response)
}

#'@name binance_dapi_ping
#'@rdname binance_dapi_ping
#'@description see
#'
#' @examples
#' # Test the connection
#' binance_dapi_ping()
#' @export

binance_dapi_ping <- function(){

  url <- "https://dapi.binance.com"

  response <- FALSE

  api_path <- c("dapi", "v1", "ping" )

  ping <- Api(url = url, path = api_path)

  response <- ifelse(purrr::is_empty(ping), TRUE, FALSE)

  attr(response, "IpWgt") <- 1
  attr(response, "api") <- "COIN-M"
  return(response)
}

#'@name binance_eapi_ping
#'@rdname binance_eapi_ping
#'@description see
#'
#'@examples
#' # Test the connection
#' binance_eapi_ping()
#' @export

binance_eapi_ping <- function(){

  url <- "https://eapi.binance.com"

  response <- FALSE

  api_path <- c("eapi", "v1", "ping" )

  ping <- Api(url = url, path = api_path)

  response <- ifelse(purrr::is_empty(ping), TRUE, FALSE)

  attr(response, "IpWgt") <- 1
  attr(response, "api") <- "OPTIONS"

  return(response)
}

#' @description Test connectivity to the Rest API and get the current server time.
#'
#' @return Returns a `POSIXct` object that represent the time of the Server.
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#' @rdname binance_api_server
#' @name binance_api_server
#'
#' @examples
#'
#' # Get the time of the server (Spot)
#' binance_api_server()
#' @export

# --------------------------------         server        ------------------------------------------------------

binance_api_server <- function(){

  url <- "https://api.binance.com"

  response <- NULL

  api_path <- c("api", "v3", "time" )

  response <- Api(url = url, path = api_path)

  if(is.null(response)){
    response <- ""
  } else {
    response <- as.POSIXct(response$serverTime/1000, origin = "1970-01-01")
  }

  attr(response, "IpWgt") <- 1
  attr(response, "api") <- "SPOT"

  return(response)

}

#'@name binance_fapi_server
#'@rdname binance_fapi_server
#'@description see
#'
#'@examples
#' # Get the time of the server
#' binance_fapi_server()
#' @export

binance_fapi_server <- function(){

  url <- "https://fapi.binance.com"

  response <- NULL

  api_path <- c("fapi", "v1", "time" )

  response <- Api(url = url, path = api_path)

  if(is.null(response)){
    response <- ""
  } else {
    response <- as.POSIXct(response$serverTime/1000, origin = "1970-01-01")
  }

  attr(response, "IpWgt") <- 1
  attr(response, "api") <- "USD-M"
  return(response)

}

#'@name binance_dapi_server
#'@rdname binance_dapi_server
#'@description see
#'
#'@examples
#' # Get the time of the server
#' binance_dapi_server()
#' @export

binance_dapi_server <- function(){

  url <- "https://dapi.binance.com"

  response <- NULL

  api_path <- c("dapi", "v1", "time" )

  response <- Api(url = url, path = api_path)

  if(is.null(response)){
    response <- ""
  } else {
    response <- as.POSIXct(response$serverTime/1000, origin = "1970-01-01")
  }

  attr(response, "IpWgt") <- 1
  attr(response, "api") <- "COIN-M"
  return(response)

}

#'@name binance_eapi_server
#'@rdname binance_eapi_server
#'@description see
#'
#'@examples
#' # Get the time of the server
#' binance_eapi_server()
#' @export

binance_eapi_server <- function(){

  url <- "https://eapi.binance.com"

  response <- NULL

  api_path <- c("eapi", "v1", "time" )

  response <- Api(url = url, path = api_path)

  if(is.null(response)){
    response <- ""
  } else {
    response <- as.POSIXct(response$serverTime/1000, origin = "1970-01-01")
  }

  attr(response, "IpWgt") <- 1
  attr(response, "api") <- "OPTIONS"
  return(response)

}

# --------------------------------         exchangeInfo        ------------------------------------------------------

#' @name binance_api_exchangeInfo
#' @rdname binance_api_exchangeInfo
#' @description Get the market informations.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param permissions only for Spot api.
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#'
#' @examples
#' # Get all the Binance's pairs
#' binance_api_exchangeInfo(pair = NULL, permissions = NULL )
#'
#' # Get all the Binance's pairs for the Spot market
#' binance_api_exchangeInfo(pair = NULL, permissions = "spot" )
#'
#' # Get all the Binance's pairs for the Margin market
#' binance_api_exchangeInfo(pair = NULL, permissions = "margin" )
#'
#' # Get all the Binance's pairs for the Leveraged market
#' binance_api_exchangeInfo(pair = NULL, permissions = "leveraged" )
#'
#' # Get the info on the pair "BTCUSDT"
#' binance_api_exchangeInfo(pair = "BTCUSDT", permissions = NULL)
#'
#' # Get the info on the pair "BTCUSDT" and "BTCBUSD"
#' binance_api_exchangeInfo(pair = c("BTCUSDT", "BNBUSDT"), permissions = NULL)
#'
#' # Get the info on the pair "BTCUSDT" and "BTCBUSD" for Margin and Leveraged Markets
#' binance_api_exchangeInfo(pair = c("BTCBUSD", "ETHBUSD"), permissions = c("margin", "leveraged"))
#'
#' @export

binance_api_exchangeInfo <- function(pair = NULL, permissions = c("all", "spot", "margin", "leveraged")){

  url <- "https://api.binance.com"

  api_path <- c("api", "v3", "exchangeInfo")

  response <- NULL

  # Multiple Pairs are allowed
  if(length(pair) > 1){

    mult_pair_name <- toupper(pair)

    # Query for Multiple Pairs
    mult_pair_query <- purrr::map_chr(mult_pair_name, ~paste0('"', .x, '"'))
    mult_pair_query <- paste0(mult_pair_query, collapse = ",")

    pair_name <- paste0('[', mult_pair_query, ']')

  } else {

    mult_pair_name <- NULL
    mult_pair_query <- NULL

  }

  # Multiple Permissions are allowed, however if a submission is inserted
  # the result must be returned for all the pairs

  if(!is.null(permissions) & sum(tolower(permissions) %in% "all") == 0){

    permissions <- tolower(permissions)
    permissions <- match.arg(permissions, choices = c("spot", "margin", "leveraged"), several.ok = TRUE)
    permissions <- toupper(permissions)

    pair_name <- NULL

    if(length(permissions) > 1){

      permissions <- purrr::map_chr(permissions, ~paste0('"', .x, '"' ))

      permissions <- paste0('[', paste0(permissions, collapse = ","), ']')
    }
  } else {
    permissions <- NULL
  }

  # Query parameter "symbol" change for multiple "symbols"
  if(length(mult_pair_name) > 1 & is.null(permissions)){

    api_query <- list(symbols = pair_name, permissions = permissions)

  } else {

    api_query <- list(symbol = NULL, permissions = permissions)

  }

  response <- Api(url = url, path = api_path, query = api_query)

  if(!purrr::is_empty(response)){

    response <- dplyr::as_tibble(response$symbols)

    # Filter for the Pairs selected
    if(length(mult_pair_name) > 1){

      response <- dplyr::filter(response, symbol %in% mult_pair_name)

    }

    # Exclude closed pairs
    # response <- dplyr::filter(response, status == "TRADING" )

    # Select only relevant variables
    # response <- dplyr::select(response, -filters, -permissions, -defaultSelfTradePreventionMode, -allowedSelfTradePreventionModes  )

    # Add the server time
    # response <- dplyr::bind_cols(Date = timestamp, response)

  } else {

    # pass for now

  }

  attr(response, "IpWgt") <- 1
  attr(response, "api") <- "SPOT"

  return(response)

}


#' @name binance_fapi_exchangeInfo
#' @rdname binance_fapi_exchangeInfo
#' @description Get the market informations.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param permissions only for Spot api.
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#'
#' @examples
#' # Futures USD-M
#' binance_fapi_exchangeInfo()
#'
#' @export

binance_fapi_exchangeInfo <- function(pair = NULL){

  response <- NULL

  url <- "https://fapi.binance.com"

  pair_name <- toupper(pair)

  api_path <- c("fapi", "v1", "exchangeInfo")

  response <- Api(url = url, path = api_path, query = NULL)

  if(!purrr::is_empty(response)){

    response <- dplyr::as_tibble(response$symbols)
    response <- dplyr::bind_cols(Market = "Futures USD-M", response)
    # Filter for the Pairs selected
    if(length(pair_name) > 1){

      response <- dplyr::filter(response, symbol %in% pair_name)

    }
  }
  attr(response, "IpWgt") <- 1
  attr(response, "api") <- "USD-M"
  return(response)

}

#' @name binance_dapi_exchangeInfo
#' @rdname binance_dapi_exchangeInfo
#' @description Get the market informations.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param permissions only for Spot api.
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#'
#' @examples
#' # Futures COIN-M
#' binance_dapi_exchangeInfo()
#'
#' @export

binance_dapi_exchangeInfo <- function(pair = NULL){

  response <- NULL

  url <- "https://dapi.binance.com"

  pair_name <- toupper(pair)

  api_path <- c("dapi", "v1", "exchangeInfo")

  response <- Api(url = url, path = api_path, query = NULL)

  if(!purrr::is_empty(response)){

    response <- dplyr::as_tibble(response$symbols)
    response <- dplyr::bind_cols(Market = "Futures COIN-M", response)

    # Filter for the Pairs selected
    if(length(pair_name) > 1){

      response <- dplyr::filter(response, symbol %in% pair_name)

    }
  }
  attr(response, "IpWgt") <- 1
  attr(response, "api") <- "COIN-M"
  return(response)

}

#' @name binance_eapi_exchangeInfo
#' @rdname binance_eapi_exchangeInfo
#' @description Get the market informations.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param permissions only for Spot api.
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#'
#' @examples
#' # OPTIONS
#' binance_eapi_exchangeInfo()
#' @export

binance_eapi_exchangeInfo <- function(pair = NULL){

  response <- NULL

  url <- "https://eapi.binance.com"

  pair_name <- toupper(pair)

  api_path <- c("eapi", "v1", "exchangeInfo")

  response <- Api(url = url, path = api_path, query = NULL)

  if(!purrr::is_empty(response)){

    response <- dplyr::as_tibble(response$optionSymbols)
    response <- dplyr::bind_cols(Market = "Options", response)

    # Filter for the Pairs selected
    if(length(pair_name) > 1){

      response <- dplyr::filter(response, symbol %in% pair_name)

    }
  }
  attr(response, "IpWgt") <- 1
  attr(response, "api") <- "OPTIONS"
  return(response)

}

# --------------------------------         lastTrades         ------------------------------------------------------

#' @name binance_api_lastTrades
#' @rdname binance_api_lastTrades
#' @description Get the most recent 1000 trades for a given pair. .
#' @param pair character containing the pair of interest. It is allowed just one symbol.
#' @return Returns a `tibble`.
#' @examples
#'
#' binance_api_lastTrades(pair = "BTCUSDT")
#'
#' @export

binance_api_lastTrades <- function(pair = "BTCUSDT"){

  url <- "https://api.binance.com"

  api_path <- c("api", "v3", "trades" )

  response <- NULL

  # Control on the Arguments
  if(is.null(pair)){
    return(NULL)
  }

  pair_name <- toupper(pair)

  api_query <- list(symbol = pair_name, limit = 1000)

  response <- Api(url = url, path = api_path, query = api_query)

  if(!is.null(response)){

    response <- dplyr::tibble(

      tradeId = response$id,
      Date = as.POSIXct(response$time/1000, origin = "1970-01-01"),
      Pair = pair_name,
      Price = as.numeric(response$price),
      Qty = as.numeric(response$qty),
      quoteQty = as.numeric(response$quoteQty),
      Side = ifelse(response$isBuyerMaker, "SELL", "BUY"),
      isBestMatch = response$isBestMatch

    )

  }

  attr(response, "IpWgt") <- 1
  attr(response, "api")   <- "SPOT"

  return(response)

}

#' @name binance_fapi_lastTrades
#' @rdname binance_fapi_lastTrades
#' @description Get the most recent 1000 trades for a given pair. .
#' @param pair character containing the pair of interest. It is allowed just one symbol.
#' @return Returns a `tibble`.
#' @examples
#'
#' binance_fapi_lastTrades(pair = "BTCUSDT")
#'
#' @export

binance_fapi_lastTrades <- function(pair = "BTCUSDT"){

  url <- "https://fapi.binance.com"

  api_path <- c("fapi", "v1", "trades" )

  response <- NULL

  # Control on the Arguments
  if(is.null(pair)){
    return(NULL)
  }

  pair_name <- toupper(pair)

  api_query <- list(symbol = pair_name, limit = 1000)

  response <- Api(url = url, path = api_path, query = api_query)

  if(!is.null(response)){

    response <- dplyr::tibble(

      tradeId = response$id,
      Date = as.POSIXct(response$time/1000, origin = "1970-01-01"),
      Pair = pair_name,
      Market = "Futures USD-M",
      Price = as.numeric(response$price),
      Qty = as.numeric(response$qty),
      quoteQty = as.numeric(response$quoteQty),
      Side = ifelse(response$isBuyerMaker, "SELL", "BUY")

    )

  }

  attr(response, "IpWgt") <- 5
  attr(response, "api")   <- "USD-M"

  return(response)

}

#' @name binance_dapi_lastTrades
#' @rdname binance_dapi_lastTrades
#' @description Get the most recent 1000 trades for a given pair. .
#' @param pair character containing the pair of interest. It is allowed just one symbol.
#' @return Returns a `tibble`.
#' @examples
#'
#' binance_dapi_lastTrades(pair = "BTCUSD_PERP")
#' @export

binance_dapi_lastTrades <- function(pair = "BTCUSD_PERP"){

  url <- "https://dapi.binance.com"

  api_path <- c("dapi", "v1", "trades" )

  response <- NULL

  # Control on the Arguments
  if(is.null(pair)){
    return(NULL)
  }

  pair_name <- toupper(pair)

  api_query <- list(symbol = pair_name, limit = 1000)

  response <- Api(url = url, path = api_path, query = api_query)

  if(!is.null(response)){

    response <- dplyr::tibble(

      tradeId = response$id,
      Date = as.POSIXct(response$time/1000, origin = "1970-01-01"),
      Pair = pair_name,
      Market = "Futures COIN-M",
      Price = as.numeric(response$price),
      Qty = as.numeric(response$qty),
      Side = ifelse(response$isBuyerMaker, "SELL", "BUY"),
    )

  }

  attr(response, "IpWgt") <- 5
  attr(response, "api")   <- "COIN-M"

  return(response)

}

#' @name binance_eapi_lastTrades
#' @rdname binance_eapi_lastTrades
#' @description Get the most recent 1000 trades for a given pair. .
#' @param pair character containing the pair of interest. It is allowed just one symbol.
#' @return Returns a `tibble`.
#' @examples
#'
#' binance_eapi_lastTrades(pair = "BTC-230224-24000-P")
#'
#' @export

binance_eapi_lastTrades <- function(pair = "BTC-230224-24000-P"){

  url <- "https://eapi.binance.com"

  api_path <- c("eapi", "v1", "trades" )

  response <- NULL

  # Control on the Arguments
  if(is.null(pair)){
    return(NULL)
  }

  pair_name <- toupper(pair)

  api_query <- list(symbol = pair_name, limit = 1000)

  response <- Api(url = url, path = api_path, query = api_query)

  if(!is.null(response)){

    response <- dplyr::tibble(

      tradeId = response$id,
      Date = as.POSIXct(response$time/1000, origin = "1970-01-01"),
      Pair = pair_name,
      Market = "Futures COIN-M",
      Price = as.numeric(response$price),
      Qty = as.numeric(response$qty),
      quoteQty = as.numeric(response$quoteQty),
      Side = ifelse(response$side == 1,"BUY", "SELL")
    )

  }

  attr(response, "IpWgt") <- 5
  attr(response, "api")   <- "OPTIONS"

  return(response)

}

# --------------------------------         price/avgPrice         ------------------------------------------------------

#' @name binance_api_price
#' @rdname binance_api_price
#' @description Symbol Price Ticker
#' @param pair character containing the pair of interest. It is allowed just one symbol.
#' @return Returns a `tibble`.
#' @examples
#'
#' binance_api_price(pair = "BTCUSDT")
#'
#' @export

binance_api_price <- function(pair = "BTCUSDT") {

  url <- "https://api.binance.com"

  response <- NULL

  api_path <- c("api", "v3", "ticker", "price" )

  pair_name <- toupper(pair)

  # Modify Query if Multiple Pairs (allowed)
  if(length(pair_name) > 1){

    mult_api_query <- purrr::map_chr(pair_name, ~paste0('"', .x, '"'))
    mult_api_query <- paste0(mult_api_query, collapse = ",")

    pair_name <- paste0('[', mult_api_query, ']')
  }

  # Query change for multiple Symbols
  if(length(pair_name) > 1){

    api_query <- list(symbols = pair_name)

  } else {

    api_query <- list(symbol = pair_name)

  }

  # Api response
  response <- Api(url = url, path = api_path, query = api_query)

  # Clean the Response
  if(!is.null(response)){

    response <- tibble::tibble(Date = Sys.time(),
                               Pair = response$symbol,
                               Price = as.numeric(response$price)
    )

  } else {

    response <- dplyr::tibble(Date = lubridate::ymd_hms(),
                              Pair = character(),
                              Price = numeric())
  }

  attr(response, "IpWgt") <- ifelse(length(pair) > 1, 2, 1)
  attr(response, "Api") <- "binance_api"

  return(response)

}

#' @name binance_api_avgPrice
#' @rdname binance_api_avgPrice
#' @description Get the average price of the last 5 minutes for a pair (or more).
#' @param pair character containing the pair of interest. It is allowed just one symbol.
#' @return Returns a `tibble`.
#' @examples
#'
#' binance_api_price(pair = "BTCUSDT")
#'
#' @export

binance_api_avgPrice <- function(pair = "BTCUSDT") {

  url <- "https://api.binance.com"

  response <- list()

  api_path <- c("api", "v3", "avgPrice" )

  # Multiple Pairs Implemented
  pair_name <- toupper(pair)

  i = 1
  for(i in 1:length(pair_name)){

    api_query <- list(symbol = pair_name[i])

    response[[i]] <- Api(url = url, path = api_path, query = api_query)

    if(!is.null(response[[i]])){

      response[[i]] <- tibble::tibble(Date = Sys.time() - 60*5,
                                      Pair = pair_name[i],
                                      Price = as.numeric(response[[i]]$price)
      )

    } else {

      response[[i]] <- dplyr::tibble(Date = lubridate::ymd_hms(),
                                     Pair = character(),
                                     Price = numeric()
      )
    }

  }

  response <- dplyr::bind_rows(response)
  attr(response, "IpWgt") <- i
  attr(response, "Api") <- "binance_api"

  return(response)
}


# --------------------------------         depth         ------------------------------------------------------

#' @rdname binance_api_depth
#' @name binance_api_depth
#' @description Get the order book for the specified pair.
#' @param pair character containing the pair of interest. It is allowed just one symbol.
#' @return Returns a `tibble`.
#' @examples
#' # Get the Order Book of the pair BTCUSDT (Spot)
#' binance_api_depth(pair = "BTCUSDT")
#'
#' @export

binance_api_depth <- function(pair = "BTCBUSD"){

  url <- "https://api.binance.com"

  response <- NULL

  api_path <- c("api", "v3", "depth" )

  # Control on the Arguments
  if(is.null(pair)){
    warning("A pair argument is mandatory!")
    return(NULL)
  }

  # Pair Name
  pair_name <- toupper(pair)

  api_query <- list(symbol = pair_name, limit = 5000)

  response <- Api(url = url, path = api_path, query = api_query)

  if(!purrr::is_empty(response)){

    # Adjust the Columns Names
    colnames(response$bids) <- c("Price", "Qty")
    colnames(response$asks) <- c("Price", "Qty")

    df_bid <- dplyr::as_tibble(response$bids)
    df_ask <- dplyr::as_tibble(response$asks)

    # Data of the Update
    update_time <- Sys.time()

    # Stucture the Order-Book
    df_bid <- dplyr::mutate(df_bid, Side = "BID", Price = as.numeric(Price), Qty = as.numeric(Qty))
    df_ask <- dplyr::mutate(df_ask, Side = "ASK", Price = as.numeric(Price), Qty = as.numeric(Qty))

    order_book <- dplyr::bind_rows(df_bid, df_ask)
    order_book <- dplyr::mutate(order_book, Date = update_time, Pair = pair_name, Market = "Spot")
    order_book <- dplyr::arrange(order_book, dplyr::desc(Price))
    order_book <- dplyr::select(order_book, Date, Market, Pair, Side, Price, Qty)

    response <- order_book

  }

  attr(response, "IpWgt") <- 50
  attr(response, "api")   <- "SPOT"

  return(response)

}

#' @rdname binance_fapi_depth
#' @name binance_fapi_depth
#' @description Get the order book for the specified pair.
#' @param pair character containing the pair of interest. It is allowed just one symbol.
#' @return Returns a `tibble`.
#' @examples
#'
#' # Get the Order Book of the pair BTCUSDT (Futures)
#' binance_fapi_depth(pair = "BTCUSDT")
#'
#' @export

binance_fapi_depth <- function(pair = "BTCUSDT"){

  url <- "https://fapi.binance.com"

  response <- NULL

  api_path <- c("fapi", "v1", "depth" )

  # Control on the Arguments
  if(is.null(pair)){
    warning("A pair argument is mandatory!")
    return(NULL)
  }

  # Pair Name
  pair_name <- toupper(pair)

  api_query <- list(symbol = pair_name, limit = 1000)

  response <- Api(url = url, path = api_path, query = api_query)

  if(!purrr::is_empty(response)){

    # Adjust the Columns Names
    colnames(response$bids) <- c("Price", "Qty")
    colnames(response$asks) <- c("Price", "Qty")

    df_bid <- dplyr::as_tibble(response$bids)
    df_ask <- dplyr::as_tibble(response$asks)

    # Data of the Update
    update_time <- Sys.time()

    # Stucture the Order-Book
    df_bid <- dplyr::mutate(df_bid, Side = "BID", Price = as.numeric(Price), Qty = as.numeric(Qty))
    df_ask <- dplyr::mutate(df_ask, Side = "ASK", Price = as.numeric(Price), Qty = as.numeric(Qty))

    order_book <- dplyr::bind_rows(df_bid, df_ask)
    order_book <- dplyr::mutate(order_book, Date = update_time, Pair = pair_name, Market = "Futures USD-M")
    order_book <- dplyr::arrange(order_book, dplyr::desc(Price))
    order_book <- dplyr::select(order_book, Date, Market, Pair, Side, Price, Qty)

    response <- order_book

  } else {

    response <- dplyr::tibble(
      Date = lubridate::ymd_hms(),
      Pair = character(),
      Side = character(),
      Price = numeric(),
      Qty = numeric()
    )
  }

  attr(response, "IpWgt") <- 20
  attr(response, "api") <- "USD-M"

  return(response)

}

#' @rdname binance_dapi_depth
#' @name binance_dapi_depth
#' @description Get the order book for the specified pair.
#' @param pair character containing the pair of interest. It is allowed just one symbol.
#' @return Returns a `tibble`.
#' @examples
#'
#' # Get the Order Book for the pair BTCUSD PERP (Futures)
#' binance_dapi_depth(pair = "BTCUSD_PERP")
#'
#' @export

binance_dapi_depth <- function(pair = "BTCUSD_PERP"){

  url <- "https://dapi.binance.com"

  response <- NULL

  api_path <- c("dapi", "v1", "depth" )

  # Control on the Arguments
  if(is.null(pair)){
    warning("A pair argument is mandatory!")
    return(NULL)
  }

  # Pair Name
  pair_name <- toupper(pair)

  api_query <- list(symbol = pair_name, limit = 1000)

  response <- Api(url = url, path = api_path, query = api_query)

  if(!purrr::is_empty(response)){

    # Adjust the Columns Names
    colnames(response$bids) <- c("Price", "Qty")
    colnames(response$asks) <- c("Price", "Qty")

    df_bid <- dplyr::as_tibble(response$bids)
    df_ask <- dplyr::as_tibble(response$asks)

    # Data of the Update
    update_time <- Sys.time()

    # Stucture the Order-Book
    df_bid <- dplyr::mutate(df_bid, Side = "BID", Price = as.numeric(Price), Qty = as.numeric(Qty))
    df_ask <- dplyr::mutate(df_ask, Side = "ASK", Price = as.numeric(Price), Qty = as.numeric(Qty))

    order_book <- dplyr::bind_rows(df_bid, df_ask)
    order_book <- dplyr::mutate(order_book, Date = update_time, Pair = pair_name, Market = "Futures COIN-M")
    order_book <- dplyr::arrange(order_book, dplyr::desc(Price))
    order_book <- dplyr::select(order_book, Date, Market, Pair, Side, Price, Qty)

    response <- order_book

  } else {

    response <- dplyr::tibble(
      Date = lubridate::ymd_hms(),
      Pair = character(),
      Side = character(),
      Price = numeric(),
      Qty = numeric()
    )
  }

  attr(response, "IpWgt") <- 20
  attr(response, "api") <- "COIN-M"

  return(response)

}

#' @rdname binance_eapi_depth
#' @name binance_eapi_depth
#' @description Get the order book for the specified pair.
#' @param pair character containing the pair of interest. It is allowed just one symbol.
#' @return Returns a `tibble`.
#' @examples
#'
#' # Get the Order Book for the Option on:
#' # BTC, strike 2400, maturity 24-02-2023
#' binance_eapi_depth(pair = "BTC-230224-24000-P")
#'
#' @export

binance_eapi_depth <- function(pair = "BTC-230224-24000-P"){

  url <- "https://eapi.binance.com"

  response <- NULL

  api_path <- c("eapi", "v1", "depth" )

  # Control on the Arguments
  if(is.null(pair)){
    warning("A pair argument is mandatory!")
    return(NULL)
  }

  # Pair Name
  pair_name <- toupper(pair)

  api_query <- list(symbol = pair_name, limit = 1000)

  response <- Api(url = url, path = api_path, query = api_query)

  if(!purrr::is_empty(response)){

    # Adjust the Columns Names
    colnames(response$bids) <- c("Price", "Qty")
    colnames(response$asks) <- c("Price", "Qty")

    df_bid <- dplyr::as_tibble(response$bids)
    df_ask <- dplyr::as_tibble(response$asks)

    # Data of the Update
    update_time <- Sys.time()

    # Stucture the Order-Book
    df_bid <- dplyr::mutate(df_bid, Side = "BID", Price = as.numeric(Price), Qty = as.numeric(Qty))
    df_ask <- dplyr::mutate(df_ask, Side = "ASK", Price = as.numeric(Price), Qty = as.numeric(Qty))

    order_book <- dplyr::bind_rows(df_bid, df_ask)
    order_book <- dplyr::mutate(order_book, Date = update_time, Pair = pair_name, Market = "European Options")
    order_book <- dplyr::arrange(order_book, dplyr::desc(Price))
    order_book <- dplyr::select(order_book, Date, Market, Pair, Side, Price, Qty)

    response <- order_book

  }

  attr(response, "IpWgt") <- 5
  attr(response, "api")   <- "OPTIONS"

  return(response)

}

# --------------------------------         aggTrades         ------------------------------------------------------

#' @rdname binance_api_aggTrades
#' @name binance_api_aggTrades
#' @description Get aggregated historical trades.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#'
#' @return Returns a `tibble` object
#'
#' @examples
#' # Get the last 1000 aggregated trades for the Pair BTCUSDT
#' binance_api_aggTrades(pair = "BTCUSDT", from = NULL, to = NULL)
#'
#' @export

binance_api_aggTrades <- function(pair = "BTCUSDT", from = NULL, to = NULL){

  url <- "https://api.binance.com"

  api_path <- c("api", "v3", "aggTrades")

  pair_name <- toupper(pair)

  # Default Start Time: last 10 minutes.
  if(is.null(from)){
    from <- Sys.time() - lubridate::minutes(10)
  } else {
    from <- as.POSIXct(from)
  }

  # Default End Time: actual time.
  if(is.null(to)){
    to <- Sys.time()
  } else {
    to <- as.POSIXct(to)
  }

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- paste0(as.integer(from), "000")
  endTime   <- as.integer(to)*1000

  while(condition){

    api_query <- list(symbol = pair_name, startTime = startTime, endTime = NULL, limit = 1000)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    colnames(response[[i]]) <- c("aggId", "Price", "Qty", "firstId",
                                 "lastId", "Date", "Side", "isBestMatch")


    last_date <- max(as.numeric(response[[i]]$Date))
    condition <- last_date < endTime
    startTime <- paste0(trunc(last_date/1000), "000")

    i = i + 1

  }

  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Date = as.POSIXct(as.numeric(Date)/1000, origin = "1970-01-01"),
                              Price = as.numeric(Price),
                              Qty = as.numeric(Qty),
                              Side = ifelse(Side, "SELL", "BUY")
    )
    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::select(response, Date, aggId, firstId, lastId,
                              Pair, Price, Qty, Side, isBestMatch)

  }

  attr(response, "IpWgt") <- i
  attr(response, "api") <- "SPOT"

  return(response)

}

#' @rdname binance_fapi_aggTrades
#' @name binance_fapi_aggTrades
#' @description Get aggregated historical trades.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#'
#' @return Returns a `tibble` object
#'
#' @examples
#' # Get aggregate trades for the BTCUSDT in Future market
#' binance_fapi_aggTrades(pair = "BTCUSDT", from = NULL, to = NULL)
#'
#' @export

binance_fapi_aggTrades <- function(pair = "BTCUSDT", from = NULL, to = NULL){

  url <- "https://fapi.binance.com"

  api_path  <- c("fapi", "v1", "aggTrades")

  # Mandatory Arguments
  if(is.null(pair)){
    warning('The "pair" argument is mandatory!')
    return(NULL)
  }

  # From date
  if(is.null(from)){
    from <- Sys.time() - lubridate::minutes(10)
  } else {
    from <- as.POSIXct(from)
  }

  # To date
  if(is.null(to)){
    to <- Sys.time()
  } else {
    to <- as.POSIXct(to)
  }

  i <- 1
  response  <- list()
  condition <- TRUE
  endTime   <- as.integer(to)*1000
  startTime <- paste0(as.integer(from), "000")

  while(condition){

    pair_name <- toupper(pair)

    api_query <- list(symbol = pair_name, startTime = startTime, endTime = NULL, limit = 1000)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    colnames(response[[i]]) <- c("aggId", "Price", "Qty", "firstId",
                                 "lastId", "Date", "Side")


    last_date <- max(as.numeric(response[[i]]$Date))
    condition <- last_date < endTime
    startTime <- paste0(trunc(last_date/1000), "000")
    i = i + 1

  }

  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- dplyr::mutate(response,
                              Pair   = toupper(pair),
                              Market = "Futures USD-M",
                              Date   = as.POSIXct(as.numeric(Date)/1000, origin = "1970-01-01"),
                              Price  = as.numeric(Price),
                              Qty    = as.numeric(Qty),
                              Side   = ifelse(Side, "SELL", "BUY")
    )

    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::select(response, Date, Market,
                              Pair, aggId, firstId,
                              lastId, Price, Qty, Side)

  }

  attr(response, "IpWgt") <- i * 20
  attr(response, "api") <- "USD-M"

  return(response)

}

#' @rdname binance_dapi_aggTrades
#' @name binance_dapi_aggTrades
#' @description Get aggregated historical trades.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#'
#' @return Returns a `tibble` object
#'
#' @examples
#' # Get the aggregated trades for the BTC in Future Coin Market
#' binance_dapi_aggTrades(pair = "BTCUSD_PERP", from = "2023-01-31 00:00:00", to = "2023-01-31 00:10:00")
#' @export

binance_dapi_aggTrades <- function(pair = "BTCUSDT", from = NULL, to = NULL){

  url <- "https://dapi.binance.com"

  api_path  <- c("dapi", "v1", "aggTrades")

  # Mandatory Arguments
  if(is.null(pair)){
    warning('The "pair" argument is mandatory!')
    return(NULL)
  }

  # From date
  if(is.null(from)){
    from <- Sys.time() - lubridate::minutes(10)
  } else {
    from <- as.POSIXct(from)
  }

  # To date
  if(is.null(to)){
    to <- Sys.time()
  } else {
    to <- as.POSIXct(to)
  }

  i <- 1
  response  <- list()
  condition <- TRUE
  endTime   <- as.integer(to)*1000
  startTime <- paste0(as.integer(from), "000")

  while(condition){

    pair_name <- toupper(pair)

    api_query <- list(symbol = pair_name, startTime = startTime, endTime = NULL, limit = 1000)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    colnames(response[[i]]) <- c("aggId", "Price", "Qty", "firstId",
                                 "lastId", "Date", "Side")


    last_date <- max(as.numeric(response[[i]]$Date))
    condition <- last_date < endTime
    startTime <- paste0(trunc(last_date/1000), "000")
    i = i + 1

  }

  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- dplyr::mutate(response,
                              Pair   = toupper(pair),
                              Market = "Futures COIN-M",
                              Date   = as.POSIXct(as.numeric(Date)/1000, origin = "1970-01-01"),
                              Price  = as.numeric(Price),
                              Qty    = as.numeric(Qty),
                              Side   = ifelse(Side, "SELL", "BUY")
    )

    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::select(response, Date, Market,
                              Pair, aggId, firstId,
                              lastId, Price, Qty, Side)

  }

  attr(response, "IpWgt") <- i * 20
  attr(response, "api") <- "COIN-M"

  return(response)

}

# --------------------------------         kline         ------------------------------------------------------

#' @rdname binance_api_kline
#' @name binance_api_kline
#' @description UIKlines/Kline/Candlestick Data
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param interval time interval
#' @param uiKlines FALSE
#' @param from start time
#' @param end end time
#'
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#'
#' @examples
#'
#' # Get the klines at 1 hour for the Futures on BTCUSDT
#' binance_api_kline(pair = "BTCUSDT", interval = "1h", from = NULL, to = NULL)
#'
#' # Get the klines at 1 day for the Futures on BTCUSDT
#' binance_api_kline(pair = "BTCUSDT", interval = "1d", from = "2018-01-01", to = "2023-01-01")
#'
#' @export

binance_api_kline <- function(pair = "BTCUSDT", interval = "1m", uiKlines = FALSE, from = NULL, to = NULL){

  url <- "https://api.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  pair_name <- toupper(pair)
  uiKlines  <- ifelse(uiKlines, "uiKlines", "klines")
  api_path  <- c("api", "v3", uiKlines)
  interval  <- match.arg(interval, choices = c("1s", "1m", "3m", "5m", "15m",
                                               "30m","1h", "2h", "4h", "6h",
                                               "8h", "12h","1d", "3d", "1w",
                                               "1M"))

  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")

  while(condition){

    api_query <- list(symbol = pair_name, startTime = NULL , interval = interval, endTime = endTime, limit = 1000)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    colnames(response[[i]]) <- c("Date", "Open", "High", "Low", "Close", "Volume",
                                 "dateClose", "volumeQuote", "Trades", "takerBuy",
                                 "takerBuyQuote", "Ignore")
    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$Date))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")

    i = i + 1
  }

  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "Spot",
                              Date = as.POSIXct(as.numeric(Date)/1000, origin = "1970-01-01"),
                              dateClose = as.POSIXct(as.numeric(dateClose)/1000, origin = "1970-01-01"),
                              Open = as.numeric(Open),
                              High = as.numeric(High),
                              Low = as.numeric(Low),
                              Close = as.numeric(Close),
                              Volume = as.numeric(Volume),
                              volumeQuote = as.numeric(volumeQuote),
                              Trades = as.numeric(Trades),
                              takerBuy = as.numeric(takerBuy),
                              takerBuyQuote = as.numeric(takerBuyQuote)
    )

    # Remove Ignore Column
    response <- dplyr::select(response, -Ignore)

    # Filter to be exactly in the "from"-"to" range
    response <- dplyr::filter(response, Date >= from & Date <= to)

    # Reorder the columns
    response <- dplyr::select(response, Date, dateClose, Market, Pair, dplyr::everything())

    # Arrange with respect to the Date
    response <- dplyr::arrange(response, Date)

  }

  # Weigth and Api attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "SPOT"

  return(response)

}

#' @rdname binance_fapi_kline
#' @name binance_fapi_kline
#' @description UIKlines/Kline/Candlestick Data
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param interval time interval
#' @param contractType NULL
#' @param continuousKlines FALSE
#' @param from start time
#' @param end end time
#'
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#'
#' @examples
#'
#' # Get the klines at 1 minute for the Futures on BTCUSDT
#' binance_fapi_kline(pair = "BTCUSDT",  interval = "1m", from = "2023-01-01 00:00:00", to = "2023-01-01 00:10:00")
#'
#' @export

binance_fapi_kline <- function(pair = "BTCUSDT", interval = "1m", contractType = NULL, continuousKlines = FALSE, from = NULL, to = NULL){

  url <- "https://fapi.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  # Match args: contractType
  if(!is.null(contractType)){
    contractType <- match.arg(contractType, choices = c("PERPETUAL", "CURRENT_QUARTER", "NEXT_QUARTER"))
  }

  # Control: IF continuousKlines = TRUE AND contractType is NULL -> PERPETUAL
  if(continuousKlines & is.null(contractType)){
    warning('The default contract type used is "PERPETUAL"')
    contractType <- "PERPETUAL"
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("fapi", "v1", ifelse(continuousKlines, "continuousKlines", "klines"))
  interval  <- match.arg(interval, choices = c("1s", "1m", "3m", "5m", "15m",
                                               "30m","1h", "2h", "4h", "6h",
                                               "8h", "12h","1d", "3d", "1w",
                                               "1M"))
  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")

  # Importation Loop
  while(condition){

    if(continuousKlines){
      api_query <- list(pair = pair_name, contractType = contractType, startTime = NULL , interval = interval, endTime = endTime, limit = 1500)
    } else {
      api_query <- list(symbol = pair_name, startTime = NULL , interval = interval, endTime = endTime, limit = 1500)
    }

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    colnames(response[[i]]) <- c("Date", "Open", "High", "Low", "Close", "Volume",
                                 "dateClose", "volumeQuote", "Trades", "takerBuy",
                                 "takerBuyQuote", "Ignore")

    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$Date))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")

    i = i + 1
  }

  # Adjust the Response
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "Futures USD-M",
                              Date = as.POSIXct(as.numeric(Date)/1000, origin = "1970-01-01"),
                              dateClose = as.POSIXct(as.numeric(dateClose)/1000, origin = "1970-01-01"),
                              Open = as.numeric(Open),
                              High = as.numeric(High),
                              Low = as.numeric(Low),
                              Close = as.numeric(Close),
                              Volume = as.numeric(Volume),
                              volumeQuote = as.numeric(volumeQuote),
                              Trades = as.numeric(Trades),
                              takerBuy = as.numeric(takerBuy),
                              takerBuyQuote = as.numeric(takerBuyQuote)
    )

    response <- dplyr::select(response, -Ignore)
    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::select(response, Date, dateClose, Market, Pair, dplyr::everything())
    response <- dplyr::arrange(response, Date)
  }

  # Attributes
  attr(response, "IpWgt") <- i * 10
  attr(response, "api") <- "USD-M"

  return(response)

}

#' @rdname binance_dapi_kline
#' @name binance_dapi_kline
#' @description UIKlines/Kline/Candlestick Data
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param interval time interval
#' @param contractType NULL
#' @param continuousKlines FALSE
#' @param from start time
#' @param end end time
#'
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#'
#' @examples
#'
#' # Get the klines at 1 minute for the Futures on BTCUSDT
#' binance_dapi_kline(pair = "BTCUSD_PERP",  interval = "1m", from = "2023-01-01 00:00:00", to = "2023-01-01 00:10:00")
#'
#' @export

binance_dapi_kline <- function(pair = "BTCUSD_PERP", interval = "1m", contractType = NULL, continuousKlines = FALSE, from = NULL, to = NULL){

  url <- "https://dapi.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  # Match args: contractType
  if(!is.null(contractType)){
    contractType <- match.arg(contractType, choices = c("PERPETUAL", "CURRENT_QUARTER", "NEXT_QUARTER"))
  }

  # Control: IF continuousKlines = TRUE AND contractType is NULL -> PERPETUAL
  if(continuousKlines & is.null(contractType)){
    warning('The default contract type used is "PERPETUAL"')
    contractType <- "PERPETUAL"
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("dapi", "v1", ifelse(continuousKlines, "continuousKlines", "klines"))
  interval  <- match.arg(interval, choices = c("1s", "1m", "3m", "5m", "15m",
                                               "30m","1h", "2h", "4h", "6h",
                                               "8h", "12h","1d", "3d", "1w",
                                               "1M"))
  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")

  # Importation Loop
  while(condition){

    if(continuousKlines){
      api_query <- list(pair = pair_name, contractType = contractType, startTime = NULL , interval = interval, endTime = endTime, limit = 1500)
    } else {
      api_query <- list(symbol = pair_name, startTime = NULL , interval = interval, endTime = endTime, limit = 1500)
    }

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    colnames(response[[i]]) <- c("Date", "Open", "High", "Low", "Close", "Volume",
                                 "dateClose", "volumeQuote", "Trades", "takerBuy",
                                 "takerBuyQuote", "Ignore")

    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$Date))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")

    i = i + 1
  }

  # Adjust the Response
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "Futures COIN-M",
                              Date = as.POSIXct(as.numeric(Date)/1000, origin = "1970-01-01"),
                              dateClose = as.POSIXct(as.numeric(dateClose)/1000, origin = "1970-01-01"),
                              Open = as.numeric(Open),
                              High = as.numeric(High),
                              Low = as.numeric(Low),
                              Close = as.numeric(Close),
                              Volume = as.numeric(Volume),
                              volumeQuote = as.numeric(volumeQuote),
                              Trades = as.numeric(Trades),
                              takerBuy = as.numeric(takerBuy),
                              takerBuyQuote = as.numeric(takerBuyQuote)
    )

    response <- dplyr::select(response, -Ignore)
    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::select(response, Date, dateClose, Market, Pair, dplyr::everything())
    response <- dplyr::arrange(response, Date)
  }

  # Attributes
  attr(response, "IpWgt") <- i * 10
  attr(response, "api") <- "COIN-M"

  return(response)

}

#' @rdname binance_eapi_kline
#' @name binance_eapi_kline
#' @description UIKlines/Kline/Candlestick Data
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param interval time interval
#' @param from start time
#' @param end end time
#'
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#'
#' @examples
#'
#' # Get the klines at 1 hour for an Option on BTCUSDT
#' binance_eapi_kline(pair = "BTC-230224-24000-P", interval = "1h", from = "2023-01-01", to = "2023-02-03")
#'
#' @export

binance_eapi_kline <- function(pair = "BTC-230224-24000-P", interval = "1m", from = NULL, to = NULL){

  url <- "https://eapi.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("eapi", "v1","klines")
  interval  <- match.arg(interval, choices = c("1s", "1m", "3m", "5m", "15m",
                                               "30m","1h", "2h", "4h", "6h",
                                               "8h", "12h","1d", "3d", "1w",
                                               "1M"))
  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")
  previous_date <- startTime

  # Importation Loop
  while(condition){

    api_query <- list(symbol = pair_name, startTime = NULL , interval = interval, endTime = endTime, limit = 1500)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }
    response[[i]] <- new_data
    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$openTime))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime & first_date != previous_date

    previous_date <- first_date

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")
    #print(endTime)
    #print(condition)
    i = i + 1
  }

  # Adjust the Response
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "Options",
                              openTime = as.POSIXct(as.numeric(openTime)/1000, origin = "1970-01-01"),
                              closeTime = as.POSIXct(as.numeric(closeTime)/1000, origin = "1970-01-01"),
                              open = as.numeric(open),
                              high = as.numeric(high),
                              low = as.numeric(low),
                              close = as.numeric(close),
                              volume = as.numeric(volume),
                              tradeCount = as.numeric(tradeCount),
                              takerVolume = as.numeric(takerVolume),
                              takerAmount = as.numeric(takerAmount),
                              amount = as.numeric(amount)
    )

    response <- dplyr::select(response, Date = "openTime", dateClose = "closeTime", Market, Pair, Open = "open",
                              High = "high", Low = "low", Close = "close", Volume = "volume", Trades = "tradeCount",
                              takerVolume, takerAmount, Amount = "amount")

    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::arrange(response, Date)
  }

  # Attributes
  attr(response, "IpWgt") <- i * 1
  attr(response, "api") <- "OPTIONS"

  return(response)

}

# --------------------------------         ticker24h         ------------------------------------------------------

#' @rdname binance_api_ticker24h
#' @name binance_api_ticker24h
#' @description 24hr Ticker Price Change Statistics.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param type the type of ticker: can be full or mini
#'
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#'
#' @examples
#'
#' binance_api_ticker24h(pair = "BTCUSDT", type = "full")
#'
#' binance_api_ticker24h(pair = "BTCUSDT", type = "mini")
#'
#' @export

binance_api_ticker24h <- function(pair = "BTCUSDT", type = c("full", "mini")) {

  response <- NULL

  url <- "https://api.binance.com"

  api_path <- c("api", "v3", "ticker", "24hr" )

  # Control on the Pair
  if(is.null(pair)){
    wrn <- 'The "pair" argument cannot be NULL'
    if(!quiet) warning(wrn)
    return(NULL)
  } else {
    pair_name <- toupper(pair)
  }

  # Control on the Type
  if(is.null(type)){
    wrn <- 'The "type" argument cannot be NULL, by default is "full".'
    if(!quiet) warning(wrn)
    type <- "full"
  } else {
    # Match type: full or mini
    type <- match.arg(tolower(type), choices = c("full", "mini"))
  }

  # Multiple Pairs allowed
  if(length(pair_name) > 1){

    mult_api_query <- purrr::map_chr(pair_name, ~paste0('"', .x, '"'))
    mult_api_query <- paste0(mult_api_query, collapse = ",")

    pair_name <- paste0('[', mult_api_query, ']')
  }

  # Query change for multiple Symbols
  if(length(pair) > 1){

    api_query <- list(symbols = pair_name, type = toupper(type))

  } else {

    api_query <- list(symbol = pair_name, type = toupper(type))

  }

  # Api Response
  response <- Api(url = url, path = api_path, query = api_query)

  # Cleaning
  if(!purrr::is_empty(response) & type == "mini"){

    response <- tibble::tibble(Date = as.POSIXct(as.numeric(response$openTime)/1000, origin = "1970-01-01"),
                               dateClose = as.POSIXct(as.numeric(response$closeTime)/1000, origin = "1970-01-01"),
                               Pair = response$symbol,
                               Open = as.numeric(response$openPrice),
                               High = as.numeric(response$highPrice),
                               Low = as.numeric(response$lowPrice),
                               Close = as.numeric(response$lastPrice),
                               Volume = as.numeric(response$volume),
                               volumeQuote = as.numeric(response$quoteVolume),
                               firstId = as.character(response$firstId),
                               lastId = as.character(response$lastId),
                               Trade = as.integer(response$count)
    )



  } else if(!purrr::is_empty(response) & type == "full"){

    response <- tibble::tibble(Date = as.POSIXct(as.numeric(response$openTime)/1000, origin = "1970-01-01"),
                               dateClose = as.POSIXct(as.numeric(response$closeTime)/1000, origin = "1970-01-01"),
                               Pair = response$symbol,
                               wgtPrice = as.numeric(response$weightedAvgPrice),
                               prevClose = as.numeric(response$prevClosePrice),
                               Open = as.numeric(response$openPrice),
                               High = as.numeric(response$highPrice),
                               Low = as.numeric(response$lowPrice),
                               Last = as.numeric(response$lastPrice),
                               Volume = as.numeric(response$volume),
                               volumeQuote = as.numeric(response$quoteVolume),
                               ASK = as.numeric(response$askPrice),
                               BID = as.numeric(response$bidPrice),
                               qtyASK = as.numeric(response$askQty),
                               qtyBID = as.numeric(response$bidQty),
                               firstId = as.character(response$firstId),
                               lastId = as.character(response$lastId),
                               Trade = as.integer(response$count)
    )

  } else {

    response <- dplyr::tibble()

  }

  # Attributes
  attr(response, "IpWgt") <- dplyr::case_when(
    length(pair) >= 1 & length(pair) <= 20 ~ 1,
    length(pair) > 20 & length(pair) <= 100 ~ 20,
    length(pair) > 100 ~ 40
  )
  attr(response, "api") <- "SPOT"

  return(response)

}

#' @rdname binance_fapi_ticker24h
#' @name binance_fapi_ticker24h
#' @description 24hr Ticker Price Change Statistics.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#'
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#'
#' @examples
#'
#' binance_fapi_ticker24h(pair = "BTCUSDT")
#'
#' @export

binance_fapi_ticker24h <- function(pair = "BTCUSDT") {

  response <- list()

  url <- "https://fapi.binance.com"

  api_path <- c("fapi", "v1", "ticker", "24hr" )

  # Control on the Pair
  if(is.null(pair)){
    wrn <- 'The "pair" argument cannot be NULL'
    if(!quiet) warning(wrn)
    return(NULL)
  } else {
    pair_name <- toupper(pair)
  }

  i <- 1
  for(i in 1:length(pair_name)){

    api_query <- list(symbol = pair_name[i])

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      next
    }

    response[[i]] <- new_data


  }

  # Cleaning
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- tibble::tibble(Date = as.POSIXct(as.numeric(response$openTime)/1000, origin = "1970-01-01"),
                               dateClose = as.POSIXct(as.numeric(response$closeTime)/1000, origin = "1970-01-01"),
                               Pair = response$symbol,
                               wgtPrice = as.numeric(response$weightedAvgPrice),
                               Open = as.numeric(response$openPrice),
                               High = as.numeric(response$highPrice),
                               Low = as.numeric(response$lowPrice),
                               Last = as.numeric(response$lastPrice),
                               Volume = as.numeric(response$volume),
                               volumeQuote = as.numeric(response$quoteVolume),
                               firstId = as.character(response$firstId),
                               lastId = as.character(response$lastId),
                               Trade = as.integer(response$count)
    )

  } else {
    response <- dplyr::tibble()
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "USD-M"

  return(response)

}

#' @rdname binance_dapi_ticker24h
#' @name binance_dapi_ticker24h
#' @description 24hr Ticker Price Change Statistics.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#'
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#'
#' @examples
#'
#' binance_dapi_ticker24h(pair = "BTCUSD_PERP")
#'
#' @export

binance_dapi_ticker24h <- function(pair = "BTCUSD_PERP") {

  response <- list()

  url <- "https://dapi.binance.com"

  api_path <- c("dapi", "v1", "ticker", "24hr" )

  # Control on the Pair
  if(is.null(pair)){
    wrn <- 'The "pair" argument cannot be NULL'
    if(!quiet) warning(wrn)
    return(NULL)
  } else {
    pair_name <- toupper(pair)
  }

  i <- 1
  for(i in 1:length(pair_name)){

    api_query <- list(symbol = pair_name[i])

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      next
    }

    response[[i]] <- new_data


  }

  # Cleaning
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- tibble::tibble(Date = as.POSIXct(as.numeric(response$openTime)/1000, origin = "1970-01-01"),
                               dateClose = as.POSIXct(as.numeric(response$closeTime)/1000, origin = "1970-01-01"),
                               Pair = response$symbol,
                               wgtPrice = as.numeric(response$weightedAvgPrice),
                               Open = as.numeric(response$openPrice),
                               High = as.numeric(response$highPrice),
                               Low = as.numeric(response$lowPrice),
                               Last = as.numeric(response$lastPrice),
                               Volume = as.numeric(response$volume),
                               firstId = as.character(response$firstId),
                               lastId = as.character(response$lastId),
                               Trade = as.integer(response$count)
    )

  } else {
    response <- dplyr::tibble()
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "COIN-M"

  return(response)

}

#' @rdname binance_eapi_ticker24h
#' @name binance_eapi_ticker24h
#' @description 24hr Ticker Price Change Statistics.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#'
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#'
#' @examples
#'
#' binance_eapi_ticker24h(pair = "BTC-230224-24000-P")
#'
#' @export

binance_eapi_ticker24h <- function(pair = "BTC-230224-24000-P") {

  response <- list()

  url <- "https://eapi.binance.com"

  api_path <- c("eapi", "v1", "ticker")

  # Control on the Pair
  if(is.null(pair)){
    wrn <- 'The "pair" argument cannot be NULL'
    if(!quiet) warning(wrn)
    return(NULL)
  } else {
    pair_name <- toupper(pair)
  }

  i <- 1
  for(i in 1:length(pair_name)){

    api_query <- list(symbol = pair_name[i])

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      next
    }

    response[[i]] <- new_data


  }

  # Cleaning
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- tibble::tibble(Date = as.POSIXct(as.numeric(response$openTime)/1000, origin = "1970-01-01"),
                               dateClose = as.POSIXct(as.numeric(response$closeTime)/1000, origin = "1970-01-01"),
                               Pair = response$symbol,
                               Open = as.numeric(response$open),
                               High = as.numeric(response$high),
                               Low = as.numeric(response$low),
                               Last = as.numeric(response$lastPrice),
                               lastVolume = as.numeric(response$lastQty),
                               ASK = as.numeric(response$askPrice),
                               BID = as.numeric(response$bidPrice),
                               Volume = as.numeric(response$volume),
                               quoteVolume = as.numeric(response$amount),
                               firstId = as.character(response$firstTradeId),
                               Trade = as.integer(response$tradeCount),
                               Strike = as.numeric(response$strikePrice),
                               excercisePrice = as.numeric(response$exercisePrice),
    )

  } else {
    response <- dplyr::tibble()
  }

  # Attributes
  attr(response, "IpWgt") <- i * 5
  attr(response, "api") <- "OPTIONS"

  return(response)

}

# --------------------------------         openInterest         ------------------------------------------------------

#' @description Open interest for a pair.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#'
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory.
#'
#' @examples
#'
#' binance_fapi_openInterest(pair = "BTCUSDT")
#'
#' binance_dapi_openInterest(pair = "BTCUSD_PERP")

binance_fapi_openInterest <- function(pair = "BTCUSDT") {

  response <- list()

  url <- "https://fapi.binance.com"

  api_path <- c("fapi", "v1", "openInterest" )

  # Control on the Pair
  if(is.null(pair)){
    wrn <- 'The "pair" argument cannot be NULL'
    if(!quiet) warning(wrn)
    return(NULL)
  } else {
    pair_name <- toupper(pair)
  }

  i <- 1
  for(i in 1:length(pair_name)){

    api_query <- list(symbol = pair_name[i])

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      next
    }

    response[[i]] <- new_data


  }

  # Cleaning
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- tibble::tibble(Date = as.POSIXct(as.numeric(response$time)/1000, origin = "1970-01-01"),
                               Market = "USD-M",
                               Pair = response$symbol,
                               openInterest = as.numeric(response$openInterest),

    )

  } else {
    response <- dplyr::tibble()
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "USD-M"

  return(response)

}

binance_dapi_openInterest <- function(pair = "BTCUSD_PERP") {

  response <- list()

  url <- "https://dapi.binance.com"

  api_path <- c("dapi", "v1", "openInterest" )

  # Control on the Pair
  if(is.null(pair)){
    wrn <- 'The "pair" argument cannot be NULL'
    if(!quiet) warning(wrn)
    return(NULL)
  } else {
    pair_name <- toupper(pair)
  }

  i <- 1
  for(i in 1:length(pair_name)){

    api_query <- list(symbol = pair_name[i])

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      next
    }

    response[[i]] <- new_data


  }

  # Cleaning
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- tibble::tibble(Date = as.POSIXct(as.numeric(response$time)/1000, origin = "1970-01-01"),
                               Market = "COIN-M",
                               Pair = response$symbol,
                               openInterest = as.numeric(response$openInterest),

    )

  } else {
    response <- dplyr::tibble()
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "COIN-M"

  return(response)

}

binance_eapi_openInterest <- function(symbol = "BTC", expiration = Sys.Date()){

  url <- "https://eapi.binance.com"

  response <- NULL

  pair_name <- toupper(symbol)
  # Compose expiration date
  expiration <- as.Date(expiration)

  year_name <- as.character(lubridate::year(expiration))
  year_name <- strsplit(year_name, "")[[1]][3:4]
  year_name <- paste0(year_name, collapse = "")

  month_name <- as.character(lubridate::month(expiration))
  month_name <- ifelse(stringr::str_length(month_name) == 2, month_name, paste0("0", month_name))

  day_name <- as.character(lubridate::day(expiration))
  day_name <- ifelse(stringr::str_length(day_name) == 2, day_name, paste0("0", day_name))


  expiration <- paste0(year_name, month_name, day_name)

  api_path  <- c("eapi", "v1", "openInterest")
  api_query <- list(underlyingAsset = pair_name, expiration = expiration)

  response <- Api(url = url, path = api_path, query = api_query)

  if(!purrr::is_empty(response)){

    response <- dplyr::as_tibble(response)

  }

  attr(response, "IpWgt") <- 1
  attr(response, "Api") <- "binance_eapi"
  return(response)

}

# --------------------------------         openInterestHist         ------------------------------------------------------


#' @description Historical Open interest for a pair.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#'
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory. The data available refers to the
#' last 30 days.
#'
#' @examples
#'
#' binance_fapi_openInterestHist(pair = "BTCUSDT", interval = "1h", from = "2023-01-20", to = Sys.time())
#'
#' binance_dapi_openInterestHist(pair = "BTCUSD", contractType = "PERPETUAL", interval = "1h", from = "2023-01-20", to = Sys.time())

binance_fapi_openInterestHist <- function(pair = "BTCUSDT", interval = "5m", from = NULL, to = NULL){

  url <- "https://fapi.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("futures","data", "openInterestHist")
  interval  <- match.arg(interval, choices = c("5m", "15m","30m","1h", "2h",
                                               "4h", "6h","12h","1d"))
  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")

  # Importation Loop
  while(condition){

    api_query <- list(symbol = pair_name, period = interval, startTime = NULL , endTime = endTime, limit = 500)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$timestamp))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")

    i = i + 1
  }

  # Adjust the Response
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "USD-M",
                              Date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              sumOpenInterest = as.numeric(sumOpenInterest),
                              sumOpenInterestValue = as.numeric(sumOpenInterestValue)
                              )

    response <- dplyr::select(response, Date, Market, Pair, sumOpenInterest, sumOpenInterestValue)
    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::arrange(response, Date)
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "USD-M"

  return(response)

}

binance_dapi_openInterestHist <- function(pair = "BTCUSDT", interval = "5m", contractType = "ALL", from = NULL, to = NULL){

  url <- "https://dapi.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  # Match args: contractType
  if(!is.null(contractType)){
    contractType <- match.arg(contractType, choices = c("ALL", "PERPETUAL", "CURRENT_QUARTER", "NEXT_QUARTER"))
  } else {
    contractType <- "ALL"
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("futures","data", "openInterestHist")
  interval  <- match.arg(interval, choices = c("5m", "15m","30m","1h", "2h",
                                               "4h", "6h","12h","1d"))
  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")

  # Importation Loop
  while(condition){

    api_query <- list(pair = pair_name, contractType = contractType, period = interval, startTime = NULL , endTime = endTime, limit = 500)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$timestamp))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")

    i = i + 1
  }

  # Adjust the Response
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "USD-M",
                              contractType = contractType,
                              Date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              sumOpenInterest = as.numeric(sumOpenInterest),
                              sumOpenInterestValue = as.numeric(sumOpenInterestValue)
    )

    response <- dplyr::select(response, Date, Market, contractType, Pair, sumOpenInterest, sumOpenInterestValue)
    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::arrange(response, Date)
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "COIN-M"

  return(response)

}

# --------------------------------         Long/Short         ------------------------------------------------------

#' @description Top Traders Long Short Historical (Accounts)
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param interval the reference interval of time.
#' @param from the start date
#' @param to the end date
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory. The data available refers to the
#' last 30 days.
#'
#' @examples
#'
#' binance_fapi_topLongShortAccountRatio(pair = "BTCUSDT", interval = "1h", from = "2023-01-20", to = Sys.time())
#'
#' binance_dapi_topLongShortAccountRatio(pair = "BTCUSD", interval = "1h", from = "2023-01-20", to = Sys.time())

binance_fapi_topLongShortAccountRatio <- function(pair = "BTCUSDT", interval = "5m", from = NULL, to = NULL){

  url <- "https://fapi.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("futures","data", "topLongShortAccountRatio")
  interval  <- match.arg(interval, choices = c("5m", "15m","30m","1h", "2h",
                                               "4h", "6h","12h","1d"))
  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")

  # Importation Loop
  while(condition){

    api_query <- list(symbol = pair_name, period = interval, startTime = NULL , endTime = endTime, limit = 500)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$timestamp))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")

    i = i + 1
  }

  # Adjust the Response
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "USD-M",
                              Date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              longShortRatio = as.numeric(longShortRatio),
                              longAccount = as.numeric(longAccount),
                              shortAccount = as.numeric(shortAccount)
    )

    response <- dplyr::select(response, Date, Market, Pair, longAccount, shortAccount, longShortRatio)
    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::arrange(response, Date)
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "USD-M"

  return(response)

}

binance_dapi_topLongShortAccountRatio <- function(pair = "BTCUSD", interval = "5m", from = NULL, to = NULL){

  url <- "https://dapi.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("futures","data", "topLongShortAccountRatio")
  interval  <- match.arg(interval, choices = c("5m", "15m","30m","1h", "2h",
                                               "4h", "6h","12h","1d"))
  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")

  # Importation Loop
  while(condition){

    # PARAM CHANGE: "pair" instead of "symbol"
    api_query <- list(pair = pair_name, period = interval, startTime = NULL , endTime = endTime, limit = 500)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$timestamp))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")

    i = i + 1
  }

  # Adjust the Response
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "COIN-M",
                              Date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              longShortRatio = as.numeric(longShortRatio),
                              longAccount = as.numeric(longAccount),
                              shortAccount = as.numeric(shortAccount)
    )

    response <- dplyr::select(response, Date, Market, Pair, longAccount, shortAccount, longShortRatio)
    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::arrange(response, Date)
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "COIN-M"

  return(response)

}



#' @description Top Traders Long Short Historical statistics (Positions)
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param interval the reference interval of time.
#' @param from the start date
#' @param to the end date
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory. The data available refers to the
#' last 30 days.
#'
#' @examples
#'
#' binance_fapi_topLongShortPositionRatio(pair = "BTCUSDT", interval = "1h", from = "2023-01-20", to = Sys.time())
#'
#' binance_dapi_topLongShortPositionRatio(pair = "BTCUSD", interval = "1h", from = "2023-01-20", to = Sys.time())

binance_fapi_topLongShortPositionRatio <- function(pair = "BTCUSDT", interval = "5m", from = NULL, to = NULL){

  url <- "https://fapi.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("futures","data", "topLongShortPositionRatio")
  interval  <- match.arg(interval, choices = c("5m", "15m","30m","1h", "2h",
                                               "4h", "6h","12h","1d"))
  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")

  # Importation Loop
  while(condition){

    api_query <- list(symbol = pair_name, period = interval, startTime = NULL , endTime = endTime, limit = 500)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$timestamp))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")

    i = i + 1
  }

  # Adjust the Response
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "USD-M",
                              Date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              longShortRatio = as.numeric(longShortRatio),
                              longAccount = as.numeric(longAccount),
                              shortAccount = as.numeric(shortAccount)
    )

    response <- dplyr::select(response, Date, Market, Pair, longAccount, shortAccount, longShortRatio)
    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::arrange(response, Date)
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "USD-M"

  return(response)

}

binance_dapi_topLongShortPositionRatio <- function(pair = "BTCUSD", interval = "5m", from = NULL, to = NULL){

  url <- "https://dapi.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("futures","data", "topLongShortPositionRatio")
  interval  <- match.arg(interval, choices = c("5m", "15m","30m","1h", "2h",
                                               "4h", "6h","12h","1d"))
  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")

  # Importation Loop
  while(condition){

    # PARAM CHANGE: "pair" instead of "symbol"
    api_query <- list(pair = pair_name, period = interval, startTime = NULL , endTime = endTime, limit = 500)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$timestamp))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")

    i = i + 1
  }

  # Adjust the Response
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "COIN-M",
                              Date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              longShortRatio = as.numeric(longShortRatio),
                              longAccount = as.numeric(longPosition),  # name Change longPosition instead of longAccount
                              shortAccount = as.numeric(shortPosition) # name Change shortPosition instead of shortAccount
    )

    response <- dplyr::select(response, Date, Market, Pair, longAccount, shortAccount, longShortRatio)
    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::arrange(response, Date)
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "COIN-M"

  return(response)

}


#' @description Global Long Short Historical statistics
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param interval the reference interval of time.
#' @param from the start date
#' @param to the end date
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory. The data available refers to the
#' last 30 days.
#'
#' @examples
#'
#' binance_fapi_globalLongShortAccountRatio(pair = "BTCUSDT", interval = "1h", from = "2023-01-20", to = Sys.time())
#'
#' binance_dapi_globalLongShortAccountRatio(pair = "BTCUSD", interval = "1h", from = "2023-01-20", to = Sys.time())
#'

binance_fapi_globalLongShortAccountRatio <- function(pair = "BTCUSDT", interval = "5m", from = NULL, to = NULL){

  url <- "https://fapi.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("futures","data", "globalLongShortAccountRatio")
  interval  <- match.arg(interval, choices = c("5m", "15m","30m","1h", "2h",
                                               "4h", "6h","12h","1d"))
  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")

  # Importation Loop
  while(condition){

    api_query <- list(symbol = pair_name, period = interval, startTime = NULL , endTime = endTime, limit = 500)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$timestamp))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")

    i = i + 1
  }

  # Adjust the Response
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "USD-M",
                              Date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              longShortRatio = as.numeric(longShortRatio),
                              longAccount = as.numeric(longAccount),
                              shortAccount = as.numeric(shortAccount)
    )

    response <- dplyr::select(response, Date, Market, Pair, longAccount, shortAccount, longShortRatio)
    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::arrange(response, Date)
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "USD-M"

  return(response)

}

binance_dapi_globalLongShortAccountRatio <- function(pair = "BTCUSD", interval = "5m", from = NULL, to = NULL){

  url <- "https://dapi.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("futures","data", "globalLongShortAccountRatio")
  interval  <- match.arg(interval, choices = c("5m", "15m","30m","1h", "2h",
                                               "4h", "6h","12h","1d"))
  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")

  # Importation Loop
  while(condition){

    # PARAM CHANGE: "pair" instead of "symbol"
    api_query <- list(pair = pair_name, period = interval, startTime = NULL , endTime = endTime, limit = 500)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$timestamp))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")

    i = i + 1
  }

  # Adjust the Response
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "USD-M",
                              Date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              longShortRatio = as.numeric(longShortRatio),
                              longAccount = as.numeric(longAccount),
                              shortAccount = as.numeric(shortAccount)
    )

    response <- dplyr::select(response, Date, Market, Pair, longAccount, shortAccount, longShortRatio)
    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::arrange(response, Date)
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "USD-M"

  return(response)

}


#' @description Global Long Short Historical statistics
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param interval the reference interval of time.
#' @param from the start date
#' @param to the end date
#' @return Returns a `tibble` object
#'
#' @details The IP Weight for this call is 1. The Data-Source is the memory. The data available refers to the
#' last 30 days.
#'
#' @examples
#'
#' binance_fapi_takerBuySellVol(pair = "BTCUSDT", interval = "1h", from = "2023-01-20", to = Sys.time())
#'
#' binance_dapi_takerBuySellVol(pair = "BTCUSD", interval = "1h", contractType = "ALL", from = "2023-01-20", to = Sys.time())

binance_fapi_takerBuySellVol <- function(pair = "BTCUSDT", interval = "5m", from = NULL, to = NULL){

  url <- "https://fapi.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("futures","data", "takerlongshortRatio")
  interval  <- match.arg(interval, choices = c("5m", "15m","30m","1h", "2h",
                                               "4h", "6h","12h","1d"))
  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")

  # Importation Loop
  while(condition){

    api_query <- list(symbol = pair_name, period = interval, startTime = NULL , endTime = endTime, limit = 500)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$timestamp))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")

    i = i + 1
  }

  # Adjust the Response
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "USD-M",
                              Date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              buySellRatio = as.numeric(buySellRatio),
                              buyVol = as.numeric(buyVol),
                              sellVol = as.numeric(sellVol)
    )

    response <- dplyr::select(response, Date, Market, Pair, buyVol, sellVol, buySellRatio)
    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::arrange(response, Date)
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "USD-M"

  return(response)

}

binance_dapi_takerBuySellVol <- function(pair = "BTCUSDT", interval = "5m", contractType = "ALL", from = NULL, to = NULL){

  url <- "https://dapi.binance.com"

  response <- list()

  # Control: pair
  if(is.null(pair)){
    warning('The argument "pair" is NULL')
    return(NULL)
  }

  # Control: interval
  if(is.null(interval)){
    warning('The argument "interval" is NULL')
    return(NULL)
  }

  # Match args: contractType
  if(!is.null(contractType)){
    contractType <- match.arg(contractType, choices = c("ALL", "PERPETUAL", "CURRENT_QUARTER", "NEXT_QUARTER"))
  } else {
    contractType <- "ALL"
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("futures","data", "takerBuySellVol")
  interval  <- match.arg(interval, choices = c("5m", "15m","30m","1h", "2h",
                                               "4h", "6h","12h","1d"))
  # Control: from
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(1)
    wrn1 <- paste0('The "from" argument is NULL, default is ', from)
    warning(wrn1)
  }

  # Control: to
  if(is.null(to)){
    to   <- Sys.time()
    wrn2 <- paste0('The "to" argument is NULL, default is ', to)
    warning(wrn2)
  }

  # Convert to POSIXct
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  i <- 1
  response  <- list()
  condition <- TRUE
  startTime <- as.integer(from)*1000
  endTime   <- paste0(trunc(as.integer(to)), "000")

  # Importation Loop
  while(condition){

    api_query <- list(pair = pair_name, contractType = contractType, period = interval, startTime = NULL , endTime = endTime, limit = 500)

    new_data <- Api(url = url, path = api_path, query = api_query)

    if(purrr::is_empty(new_data)){
      return(dplyr::tibble())
    }

    response[[i]] <- new_data

    response[[i]] <- dplyr::as_tibble(response[[i]])

    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$timestamp))

    # Condition: IF first_date is greater than startTime -> STOP
    condition <- first_date > startTime

    # ELSE: use the first_date as new endTime
    endTime <- paste0(trunc(first_date/1000), "000")

    i = i + 1
  }

  # Adjust the Response
  if(!purrr::is_empty(response)){

    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              Pair = pair_name,
                              Market = "USD-M",
                              contractType = contractType,
                              Date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              takerBuyVol = as.numeric(takerBuyVol),
                              takerBuyVolValue = as.numeric(takerBuyVolValue),
                              takerSellVol = as.numeric(takerSellVol),
                              takerSellVolValue = as.numeric(takerSellVolValue)
    )

    response <- dplyr::select(response, Date, Market, contractType, Pair, takerBuyVol, takerSellVol, takerBuyVolValue, takerSellVolValue)
    response <- dplyr::filter(response, Date >= from & Date <= to)
    response <- dplyr::arrange(response, Date)
  }

  # Attributes
  attr(response, "IpWgt") <- i
  attr(response, "api") <- "COIN-M"

  return(response)

}


