#' @name binance_vision_spot_kline
#' @rdname binance_vision_spot_kline
#' @description Historical Kline/Candlestick Data for a pair from the database.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param day_date a character or a Date object containing the date of witch we would like to obtain the data. Note
#' that the data available refer to the day before. For more recent data is better to use the REST api or the WebSocket.
#' @param interval character object containing the time interval, for example `1m` for 1 minute data, `1h` for 1 hour data.
#'
#' @param quiet logical, if TRUE some messages will be displayed.
#'
#' @return Returns a `tibble` object
#'
#' @examples
#'
#' ## Get the klines at 1 minute for the spot pair BTCUSDT
#' binance_vision_spot_kline(pair = "BTCUSDT", day_date = "2023-01-01", interval = "1m", quiet = FALSE)
#'
#' binance_vision_futures_kline(pair = "BTCUSDT", method = "klines", market = "USD-m")
#' binance_vision_futures_kline(pair = "BTCUSDT", method = "indexPriceKlines", market = "USD-m")
#' binance_vision_futures_kline(pair = "BTCUSDT", method = "markPriceKlines", market = "USD-m", day_date = "2020-01-15")
#' binance_vision_futures_kline(pair = "BTCUSDT", method = "premiumIndexKlines", market = "USD-m", interval = "1m", day_date = "2020-01-15")
#'
#' @export

binance_vision_spot_kline <- function(pair = "BTCUSDT", day_date = Sys.Date()-2, interval = "1m", quiet = FALSE){

  # Pair and Interval
  pair_name <- toupper(pair)
  interval <- match.arg(interval, choices = c("1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d"))

  # Check default Date
  if(is.null(day_date) || day_date > (Sys.Date()-2)){

    day_date <- Sys.Date()-2
    wrn <- paste0('The maximum date available is: ', day_date)
    if(!quiet) warning(wrn)

  } else {

    day_date <- as.Date(day_date)

  }

  # Url Binance Vision: Spot
  base_url <- "https://data.binance.vision/data/spot/daily/klines"

  # Dataset Url
  data_url <- paste0(base_url, "/", pair_name, "/", interval, "/", pair_name, "-", interval, "-", day_date, ".zip")

  # File Name in Temp (as csv file)
  file_name <- paste0(pair_name, "-", interval, "-", day_date, ".csv")

  # Create a temporary directory
  temp <- tempfile()

  # Download the file
  safe_download <- purrr::safely(download.file)

  dwn <- safe_download(data_url, temp, quiet = quiet)

  if(is.null(dwn$result)){
    warning("Some errors with the download of the file")
    return(dplyr::tibble())
  }

  # OpenTime X1
  # CloseTime X7
  # Open X2
  # High X3
  # Low X4
  # Close X5
  # Volume X6
  # Quote asset Volume X8
  # Number of Trades X9
  # Taker buy base asset volume X10
  # Taker buy quote asset volume X11
  # Ignore X12

  # Unzip and Read the file
  response <- unz(temp, file_name)
  response <- readr::read_csv(response,
                              show_col_types = FALSE, # Avoid messages
                              col_names = c("Date", "Open", "High", "Low", "Close",
                                            "Volume", "dateClose", "volumeQuote", "Trades",
                                            "takerBuy", "takerBuyQuote", "Ignore"))

  # Unlink the connection created with temp
  unlink(temp)

  # Clear the response
  response <- dplyr::mutate(response,
                            Date = as.POSIXct(Date/1000, origin = "1970-01-01"),
                            dateClose = as.POSIXct(dateClose/1000, origin = "1970-01-01"),
                            Market = "Spot",
                            Pair = pair_name,
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

  # Reorder the variables
  response <- dplyr::select(response,
                            Date, dateClose, Market, Pair,
                            Open, High, Low, Close, Volume,
                            volumeQuote, Trades, takerBuy, takerBuyQuote)

  # Arrange by Date (from the most recent)
  response <- dplyr::arrange(response, dplyr::desc(Date))

  return(response)

}

#' @name binance_vision_futures_kline
#' @rdname binance_vision_futures_kline
#'
#' @description Historical Kline/Candlestick Data for a pair from the database.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param day_date a character or a Date object containing the date of witch we would like to obtain the data. Note
#' that the data available refer to the day before. For more recent data is better to use the REST api or the WebSocket.
#' @param interval character object containing the time interval, for example `1m` for 1 minute data, `1h` for 1 hour data.
#' @param method character vector: can be `klines`, `indexPriceKlines`, `markPriceKlines`, `premiumIndexKlines`.
#' @param market character vector: can be `USD-M` for the futures marginated in dollars and `COIN-M` for the futures marginated with the underline.
#' @param quiet logical, if TRUE some messages will be displayed.
#'
#' @return Returns a `tibble` object
#'
#' @examples
#'
#' ## Get the klines at 1 minute for the spot pair BTCUSDT
#'
#' binance_vision_futures_kline(pair = "BTCUSDT", method = "klines", market = "USD-m")
#' binance_vision_futures_kline(pair = "BTCUSDT", method = "indexPriceKlines", market = "USD-m")
#' binance_vision_futures_kline(pair = "BTCUSDT", method = "markPriceKlines", market = "USD-m", day_date = "2020-01-15")
#' binance_vision_futures_kline(pair = "BTCUSDT", method = "premiumIndexKlines", market = "USD-m", interval = "1m", day_date = "2020-01-15")
#'
#' @export

binance_vision_futures_kline <- function(pair = "BTCUSD_PERP", day_date = Sys.Date()-2, market = "COIN-M", method = "klines", interval = "1m", quiet = FALSE){

  # Arguments: Pair Name
  pair_name <- toupper(pair)

  # Arguments: Type of Future
  future_type <- toupper(market)
  future_type <- match.arg(future_type, choices = c("USD-M", "COIN-M"))
  market <- dplyr::case_when(
    future_type == "USD-M" ~ "um",
    future_type == "COIN-M" ~ "cm"
  )

  # Arguments: Method
  method <- match.arg(method, choices = c("klines", "indexPriceKlines", "markPriceKlines", "premiumIndexKlines"))

  # Arguments: Interval
  interval <- tolower(interval)
  interval <- match.arg(interval, choices = c("1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d", "3d", "1w", "1mo"))

  # Check default Date
  if(is.null(day_date) || day_date > (Sys.Date()-2)){

    day_date <- Sys.Date()-2
    wrn <- paste0('The maximum date available is: ', day_date)
    if(!quiet) warning(wrn)

  } else {

    day_date <- as.Date(day_date)

  }

  # Url Binance Vision: Futures
  base_url <- "https://data.binance.vision/data/futures"

  # Dataset Url
  data_url <- paste0(base_url, "/", market, "/", "daily", "/", method, "/", pair_name, "/", interval, "/", pair_name, "-", interval, "-", day_date, ".zip")

  # File Name in Temp (as csv file)
  file_name <- paste0(pair_name, "-", interval, "-", day_date, ".csv")

  # Create a temporary directory
  temp <- tempfile()

  # Download the file
  download.file(data_url, temp, quiet = quiet)

  # OpenTime X1
  # CloseTime X7
  # Open X2
  # High X3
  # Low X4
  # Close X5
  # Volume X6
  # Quote asset Volume X8
  # Number of Trades X9
  # Taker buy base asset volume X10
  # Taker buy quote asset volume X11
  # Ignore X12

  # Unzip and Read the file
  response <- unz(temp, file_name)

  response <- readr::read_csv(response,
                              skip = 1,
                              show_col_types = FALSE, # Avoid messages
                              col_names = c("Date", "Open", "High", "Low", "Close", "Volume",
                                            "dateClose", "volumeQuote", "Trades", "takerBuy",
                                            "takerBuyQuote", "Ignore"))

  # Unlink the connection created with temp
  unlink(temp)

  response <- dplyr::mutate(response,
                            Date = as.numeric(Date),
                            Date = as.POSIXct(Date/1000, origin = "1970-01-01"),
                            dateClose = as.numeric(dateClose),
                            dateClose = as.POSIXct(dateClose/1000, origin = "1970-01-01"),
                            Market = future_type,
                            Pair = pair_name,
                            Method = method,
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

  # Reorder the Variables
  response <- dplyr::select(response,
                            Date, dateClose, Market, Pair, Method,
                            Open, High, Low, Close, Volume,
                            volumeQuote, Trades, takerBuy, takerBuyQuote)

  # Arrange by Date (from the most recent)
  response <- dplyr::arrange(response, dplyr::desc(Date))

  return(response)

}

#' @name binance_vision_spot_trade
#' @rdname binance_vision_spot_trade
#' @description Historical Trades Data for a pair from the database.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param day_date a character or a Date object containing the date of witch we would like to obtain the data. Note
#' that the data available refer to the day before. For more recent data is better to use the REST api or the WebSocket.
#' @param quiet logical, if TRUE some messages will be displayed.
#'
#' @return Returns a `tibble` object
#'
#' @examples
#'
#' # Get the raw trades for the spot pair BTCUSDT
#' binance_vision_spot_trade(pair = "BTCUSDT", day_date = "2023-01-01")
#'
#' # Get the raw trades for the spot pair LAZIOUSDT
#' binance_vision_spot_trade(pair = "LAZIOUSDT", day_date = "2023-01-01")
#'
#' @export

binance_vision_spot_trade <- function(pair = "BTCUSDT", day_date = Sys.Date()-2, quiet = FALSE){

  # Pair Name
  pair_name <- toupper(pair)

  # Check default Date
  if(is.null(day_date) || day_date > (Sys.Date()-2)){

    day_date <- Sys.Date()-2
    wrn <- paste0('The maximum date available is: ', day_date)
    if(!quiet) warning(wrn)

  } else {

    day_date <- as.Date(day_date)

  }


  # Url Binance Vision
  base_url <- "https://data.binance.vision/data/spot/daily/trades/"

  data_url <- paste0(base_url, pair_name, "/", pair_name, "-trades-", day_date, ".zip")

  # File Name in Temp (as csv file)
  file_name <- paste0(pair_name, "-trades-", day_date, ".csv")

  # Create a temporary directory
  temp <- tempfile()

  # Download the file
  download.file(data_url, temp, quiet = quiet)

  # tradeID X1
  # Price X2
  # Quantity X3
  # quoteQty X4
  # Date X5
  # isBuyMaker X6
  # isBestMatch X7

  # Unzip and Read the file
  response <- unz(temp, file_name)
  response <- readr::read_csv(response,
                              progress = FALSE,
                              show_col_types = FALSE, # Avoid messages
                              col_names = c("Id", "Price", "Qty", "quoteQty", "Date", "Side", "isBestMatch"))

  unlink(temp)

  response <- dplyr::mutate(response,
                            Date = as.POSIXct(Date/1000, origin = "1970-01-01"),
                            Market = "Spot",
                            Pair = pair_name,
                            Price = as.numeric(Price),
                            Qty = as.numeric(Qty),
                            quoteQty = as.numeric(quoteQty),
                            Side = ifelse(Side, "SELL", "BUY")
  )

  # Reorder the Variables
  response <- dplyr::select(response,
                            Date, Id, Market, Pair, Price, Qty,
                            quoteQty, Side, isBestMatch)

  return(response)

}

#' @name binance_vision_spot_aggTrade
#' @rdname binance_vision_spot_aggTrade
#' @description Historical Aggregated Trades Data for a pair from the Spot database.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param day_date a character or a Date object containing the date of witch we would like to obtain the data. Note
#' that the data available refer to the day before. For more recent data is better to use the REST api or the WebSocket.
#' @param quiet logical, if TRUE some messages will be displayed.
#'
#' @return Returns a `tibble` object
#'
#' @examples
#'
#' # Get the aggregated trades for the spot pair LAZIOUSDT
#' binance_vision_spot_aggTrade(pair = "LAZIOUSDT", day_date = "2023-01-01")
#'
#' binance_vision_futures_trade(pair = "BTCUSD_PERP", day_date = Sys.Date()-2, market = "COIN-M", quiet = FALSE)
#'
#' @export

binance_vision_spot_aggTrade <- function(pair = "BTCUSDT", day_date = Sys.Date()-2, quiet = FALSE){

  # Pair Name
  pair_name <- toupper(pair)

  # Check default Date
  if(is.null(day_date) || day_date > (Sys.Date()-2)){

    day_date <- Sys.Date()-2
    wrn <- paste0('The maximum date available is: ', day_date)
    if(!quiet) warning(wrn)

  } else {

    day_date <- as.Date(day_date)

  }

  # Url Binance Vision
  base_url <- "https://data.binance.vision/data/spot/daily/aggTrades/"

  # Dataset Url
  data_url <- paste0(base_url, pair_name, "/", pair_name, "-aggTrades-", day_date, ".zip")

  # File Name in Temp (as csv file)
  file_name <- paste0(pair_name, "-aggTrades-", day_date, ".csv")

  # Create a temporary directory
  temp <- tempfile()

  # Download the file
  download.file(data_url, temp, quiet = quiet)

  # tradeID X1
  # Price X2
  # Quantity X3
  # firstId X4
  # lastId X5
  # Date X6
  # isBuyMaker X6
  # isBestMatch X7

  # Unzip and Read the file
  response <- unz(temp, file_name)
  response <- readr::read_csv(response,
                              progress = FALSE,
                              show_col_types = FALSE, # Avoid messages
                              col_names = c("aggId", "Price", "Qty", "firstId", "lastId", "Date", "Side", "isBestMatch"))

  unlink(temp)

  response <- dplyr::mutate(response,
                            Date = as.POSIXct(Date/1000, origin = "1970-01-01"),
                            Market = "Spot",
                            Pair = pair_name,
                            Price = as.numeric(Price),
                            Qty = as.numeric(Qty),
                            Side = ifelse(Side, "SELL", "BUY")
  )

  # Reorder the Variables
  response <- dplyr::select(response,
                            Date, Market, Pair, aggId, firstId, lastId, Pair, Price, Qty,
                            Side, isBestMatch)
  return(response)

}


#' @name binance_vision_futures_trade
#' @rdname binance_vision_futures_trade
#' @description Historical Aggregated Trades Data for a pair from the Futures database.
#' @param pair a character containing the Pair of interest, for example `BTCUSDT`.
#' @param market character vector: can be `USD-M` for the futures marginated in dollars and `COIN-M` for the futures marginated with the underline.
#' @param quiet logical, if TRUE some messages will be displayed.
#'
#'
#' @return Returns a `tibble` object
#'
#' @examples
#'
#' # Get the aggregated trades for the futures pair BTCUSDT (USD-M)
#' binance_vision_futures_trade(pair = "BTCUSDT", day_date = Sys.Date()-2, market = "USD-M", quiet = FALSE)
#'
#' # Get the aggregated trades for the futures pair BTCUSDT (COIN-M)
#' binance_vision_futures_trade(pair = "BTCUSD_PERP", day_date = Sys.Date()-2, market = "COIN-M", quiet = FALSE)
#'
#' @export

binance_vision_futures_trade <- function(pair = "BTCUSD_PERP", day_date = Sys.Date()-2, market = "COIN-M", quiet = FALSE){

  # Pair Name
  pair_name <- toupper(pair)

  # Arguments: Type of Future
  future_type <- toupper(market)
  future_type <- match.arg(future_type, choices = c("USD-M", "COIN-M"))
  market <- dplyr::case_when(
    future_type == "USD-M" ~ "um",
    future_type == "COIN-M" ~ "cm"
  )

  # Check default Date
  if(is.null(day_date) || day_date > (Sys.Date()-2)){

    day_date <- Sys.Date()-2
    wrn <- paste0('The maximum date available is: ', day_date)
    if(!quiet) warning(wrn)

  } else {

    day_date <- as.Date(day_date)

  }


  # Url Binance Vision
  base_url <- "https://data.binance.vision/data/futures"

  data_url <- paste0(base_url, "/", market, "/", "daily/trades/", pair_name, "/", pair_name, "-trades-", day_date, ".zip")

  # File Name in Temp (as csv file)
  file_name <- paste0(pair_name, "-trades-", day_date, ".csv")

  # Create a temporary directory
  temp <- tempfile()

  # Download the file
  download.file(data_url, temp, quiet = quiet)

  # tradeID X1
  # Price X2
  # Quantity X3
  # baseQty X4
  # Date X5
  # isBuyMaker X6

  # Unzip and Read the file
  response <- unz(temp, file_name)
  response <- readr::read_csv(response,
                              progress = FALSE,
                              show_col_types = FALSE, # Avoid messages
                              skip = 1,
                              col_names = c("Id", "Price", "Qty", "baseQty", "Date", "isBuyMaker"))
  unlink(temp)

  response <- dplyr::mutate(response,
                            Date = as.POSIXct(Date/1000, origin = "1970-01-01"),
                            Market = future_type,
                            Pair = pair_name,
                            Price = as.numeric(Price),
                            Qty = as.numeric(Qty),
                            baseQty = as.numeric(baseQty)
  )

  # Reorder the Variables
  response <- dplyr::select(response,
                            Date, Market, Pair, Id, Price, Qty,
                            baseQty, isBuyMaker)
  return(response)

}

