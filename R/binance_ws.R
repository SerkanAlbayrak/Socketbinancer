#'@name binance_ws_api_socket
#'@rdname binance_ws_api_socket
#'@description api websocket
#'
#' @export
binance_ws_api_socket <- function(pair = "BTCUSDT", subscription = "kline", interval = "1m", update.speed = "1000", quiet = FALSE){

  web_socket_url <- binance_ws_api_url(pair = pair, subscription = subscription, interval = interval, update.speed = update.speed, quiet = quiet)

  ws <- websocket::WebSocket$new(web_socket_url, autoConnect = FALSE)

  # Create a new environment to save the Data
  ws$.__enclos_env__$stream <- list()

  # Slot for saving websocket data
  ws$.__enclos_env__$stream$data <- dplyr::tibble()

  ws_info <- dplyr::tibble(
    dateOpen = NA_character_,
    dateClose = NA_character_,
    Pair = toupper(pair),
    Subscription = subscription,
    Interval = interval,
    update.speed = update.speed,
    Obs = nrow(ws$.__enclos_env__$stream$data)
  )

  ws$.__enclos_env__$stream$info <- ws_info

  ws$onOpen(function(event) {

    if(!quiet) message("Stream: ", toupper(pair), "@", subscription, " opened!")

    ws$.__enclos_env__$stream$info$dateOpen = Sys.time()
  })

  ws$onMessage(function(event) {

    # Get the Old Data
    old_data <- ws$.__enclos_env__$stream$data

    # Get the New Data
    new_data <- jsonlite::fromJSON(event$data)

    # Assign the class for "binance_ws_cleaner"
    attr(new_data, "class") <- c(subscription, class(new_data))

    # Output as tibble
    new_data <- binance_ws_cleaner(new_data)

    if(subscription == "depth"){

      # If depth we substitue the old order book with the updated one
      new_data <- suppressMessages(binance_ws_orderbook(data_before = old_data[1,], data_after = new_data))

      ws$.__enclos_env__$stream$data <- new_data

    } else if(subscription == "kline"){

      # If kline we check if the candle is closed to avoid repetitions
      new_data <- binance_ws_kline(data_before = old_data, data_after = new_data)

      ws$.__enclos_env__$stream$data <- new_data

    } else {

      # In all the other cases we save the data (kline, trades and aggTrades)
      ws$.__enclos_env__$stream$data <- dplyr::bind_rows(new_data, old_data)

    }

    ws$.__enclos_env__$stream$info$Obs <- nrow(ws$.__enclos_env__$stream$data)

  })

  ws$onClose(function(event) {

    if(!quiet) message("Stream: ", toupper(pair), "@", subscription, " Closed with code ", event$code, "\n")
    ws$.__enclos_env__$stream$info$dateClose = Sys.time()
  })

  ws$onError(function(event) {

    if(!quiet) message("ERROR: Stream: ", toupper(pair), "@", subscription, " failed to connect: ", event$code, "\n")

  })

  return(ws)

}

#'@name binance_ws_fapi_socket
#'@rdname binance_ws_fapi_socket
#'@description fapi websocket
#'
#' @export
binance_ws_fapi_socket <- function(pair = "BTCUSDT", subscription = "kline", interval = "1m", contractType = NULL, update.speed = "1000", quiet = FALSE){

  web_socket_url <- binance_ws_fapi_url(pair = pair, subscription = subscription, interval = interval, contractType = contractType, update.speed = update.speed, quiet = quiet)

  ws <- websocket::WebSocket$new(web_socket_url, autoConnect = FALSE)

  # Create a new environment to save the Data
  ws$.__enclos_env__$stream <- new.env()

  # Slot for saving websocket data
  ws$.__enclos_env__$stream$data <- dplyr::tibble()

  ws_info <- dplyr::tibble(
    dateOpen = NA_character_,
    dateClose = NA_character_,
    Api = "Futures USD-M",
    Pair = toupper(pair),
    Subscription = subscription,
    Interval = interval,
    update.speed = update.speed,
    Obs = nrow(ws$.__enclos_env__$stream$data)
  )

  ws$.__enclos_env__$stream$Info <- ws_info

  ws$onOpen(function(event) {

    if(!quiet) message("Stream: ", toupper(pair), "@", subscription, " opened!")

    ws$.__enclos_env__$stream$Info$dateOpen = Sys.time()
  })

  ws$onMessage(function(event) {

    # Get the Old Data
    old_data <- ws$.__enclos_env__$stream$data

    # Get the New Data
    new_data <- jsonlite::fromJSON(event$data)

    # Assign the class for "binance_ws_cleaner"
    attr(new_data, "class") <- c(subscription, class(new_data))

    # Output as tibble
    new_data <- binance_ws_cleaner(new_data)

    if(subscription == "depth"){

      # If depth we substitue the old order book with the updated one
      new_data <- suppressMessages(binance_ws_orderbook(data_before = old_data[1,], data_after = new_data))

      ws$.__enclos_env__$stream$data <- new_data

    } else if(subscription %in% c("kline", "continuousKline")){

      # If kline we check if the candle is closed to avoid repetitions
      new_data <- binance_ws_kline(data_before = old_data, data_after = new_data)

      ws$.__enclos_env__$stream$data <- new_data

    } else {

      # In all the other cases we save the data (kline, trades and aggTrades)
      ws$.__enclos_env__$stream$data <- dplyr::bind_rows(new_data, old_data)

    }

    ws$.__enclos_env__$stream$Info$Obs <- nrow(ws$.__enclos_env__$stream$data)

  })

  ws$onClose(function(event) {

    if(!quiet) message("Stream: ", toupper(pair), "@", subscription, " Closed with code ", event$code, "\n")
    ws$.__enclos_env__$stream$Info$dateClose = Sys.time()
  })

  ws$onError(function(event) {

    if(!quiet) message("ERROR: Stream: ", toupper(pair), "@", subscription, " failed to connect: ", event$code, "\n")

  })

  return(ws)

}


#'@name binance_ws_api_url
#'@rdname binance_ws_api_url
#'@description Create the request URL for the differents APIs
#'
#' @export

binance_ws_api_url <- function(pair = "BTCUSDT", subscription = "kline", interval = "1m", update.speed = "1000", quiet = FALSE){

  # Websocket Url
  base_url <- "wss://stream.binance.com:9443/ws"

  web_socket_url <- NULL

  # Available Subscriptions
  subscription <- match.arg(subscription, choices = c("aggTrade", "trade", "depth", "miniTicker", "ticker", "bookTicker", "kline"))

  # Pair Name
  pair_name <- tolower(pair)


  if(subscription == "kline"){

    interval <- match.arg(interval, choices = c("1s", "1m", "3m", "5m", "15m", "30m", "1h",
                                                "2h", "4h", "6h", "12h", "1d", "3d", "1w", "1M"))

    web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription, "_", interval)


  } else if(subscription == "ticker" & !is.null(interval) && interval %in% c("1h", "4h", "1d")){

    # Ticker with Window Size
    web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription, "_", interval)

    msg <- paste0('The stream opened is a ticker with Window Size: ', pair_name, "@", subscription, "_", interval)

    if(!quiet) message(msg)

  } else if(subscription == "depth") {

    update.speed <- match.arg(update.speed, choices = c("100", "1000"))

    # Depth stream with a different update speed
    web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription, "@", update.speed, "ms")

    msg <- paste0('The stream opened is a Depth with Update Speed: ', pair_name, "@", subscription, "@", update.speed, "ms")

    if(!quiet) message(msg)

  } else {

    web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription)

  }


  return(web_socket_url)

}

#'@name binance_ws_fapi_url
#'@rdname binance_ws_fapi_url
#'@description Create the request URL for the differents APIs
#'
#' @export
binance_ws_fapi_url <- function(pair = "BTCUSDT", subscription = "kline", interval = "1m", contractType = NULL, update.speed = "1000", quiet = FALSE){

  # Websocket Url
  base_url <- "wss://fstream.binance.com/ws"
  web_socket_url <- NULL

  # Available Subscriptions
  subscription <- match.arg(subscription, choices = c("markPrice", "aggTrade", "depth", "forceOrder", "miniTicker", "ticker", "bookTicker", "kline", "continuousKline"))

  # Pair Name
  pair_name <- tolower(pair)


  if(subscription %in% c("kline", "continuousKline")){

    interval <- match.arg(interval, choices = c("1s", "1m", "3m", "5m", "15m", "30m", "1h",
                                                "2h", "4h", "6h", "12h", "1d", "3d", "1w", "1M"))

    if(subscription == "kline"){

      web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription, "_", interval)

    } else {

      contractType <- match.arg(tolower(contractType), choices = c("perpetual", "current_quarter", "next_quarter"))

      web_socket_url <- paste0(base_url, "/", pair_name, "_", contractType, "@", subscription, "_", interval)

    }

  } else if(subscription == "depth") {

    update.speed <- match.arg(update.speed, choices = c("100", "250", "500"))

    # Depth stream with a different update speed
    web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription, "@", update.speed, "ms")

    msg <- paste0('The stream opened is a Depth with Update Speed: ', pair_name, "@", subscription, "@", update.speed, "ms")

    if(!quiet) message(msg)

  } else {

    web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription)

  }

  return(web_socket_url)

}
