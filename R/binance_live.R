#'@name binance_live_depth
#'@rdname binance_live_depth
#'@description live orderbook
#'
#' @export

binance_live_depth <- function(pair = "BTCUSDT"){

  response <- NULL

  pair_name <- toupper(pair)

  # Websocket connection for Depth Update
  ws <- binance_ws_api_socket(pair = pair_name, subscription = "depth", interval = NULL)

  # Snapshot of the Order Book
  response <- binance_api_depth(pair = pair_name)

  # Nest to replicate the stucture in "binance_api_soocket"
  response <- dplyr::group_by(response, Side, Price, Qty)
  response <- tidyr::nest(response, order_book = c("Side", "Price", "Qty"))

  # Add the order book in the Websocket Data
  ws$.__enclos_env__$stream$data  <- response

  ws$connect()

  Data <- function(){
    ws$.__enclos_env__$stream$data
  }

  structure(
    list(
      Close = ws$close,
      Data = Data
    )
  )
}

#'@name binance_live_kline
#'@rdname binance_live_kline
#'@description live candlestick
#'
#' @export

binance_live_kline <- function(pair = "BTCUSDT", interval = "1m", data = NULL){

  pair_name <- toupper(pair)
  response <- NULL

  # Initialize with some data
  if(is.null(data)){

    from <- Sys.time()-lubridate::days(2)
    to <- Sys.time()

    response <- binance_api_kline(pair = pair_name, from = from, to = to, interval = interval )

  } else {

    response <- data
  }


  # Websocket connection for Depth Update
  ws <- binance_ws_api_socket(pair = pair_name, subscription = "kline", interval = interval)

  # Update till last Data

  new_data <- binance_api_kline(pair = pair_name, from = max(response$Date), to = Sys.time(), interval = interval )

  # Replicate the stucture in "binance_api_socket"
  response <- dplyr::bind_rows(new_data, response)
  response <- response[!duplicated(response$Date),]
  response <- dplyr::mutate(response, isClosed = TRUE)

  # Add the order book in the Websocket Data
  ws$.__enclos_env__$stream$data  <- response

  ws$connect()

  Data <- function(){
    ws$.__enclos_env__$stream$data
  }

  structure(
    list(
      Close = ws$close,
      Data = Data
    )
  )
}

#'@name binance_live_aggTrades
#'@rdname binance_live_aggTrades
#'@description live trades
#'
#' @export

binance_live_aggTrades <- function(pair = "BTCUSDT"){

  response <- NULL

  pair_name <- toupper(pair)

  # Websocket connection for Depth Update
  ws <- binance_ws_api_socket(pair = pair_name, subscription = "aggTrade", interval = NULL)

  # Snapshot of the Order Book
  response <- binance_api_aggTrades(pair = pair_name )

  # Add the order book in the Websocket Data
  ws$.__enclos_env__$stream$data  <- response

  ws$connect()

  Data <- function(){
    ws$.__enclos_env__$stream$data
  }

  structure(
    list(
      Close = ws$close,
      Data = Data
    )
  )
}

