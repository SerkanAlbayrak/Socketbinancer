#' @description Decompose the name component in structure data.
#' @return Returns a `tibble`
#' @examples
#' binance_option_info(pair = "BTC-230224-24000-P")

binance_option_info <- function(pair = "BTC-230224-24000-P"){

  pair_name <- toupper(pair)

  option_info <- strsplit(pair_name, "-")[[1]]

  # Extract Maturity, Strike and Option Type

  maturity <- strsplit(option_info[2], "")[[1]]
  maturity <- paste0(maturity[1], maturity[2], "-", maturity[3], maturity[4], "-", maturity[5], maturity[6])
  maturity <- lubridate::ymd(maturity)

  tibble(
    Symbol = option_info[1],
    Maturity = maturity,
    Strike = as.numeric(option_info[3]),
    Type = ifelse(option_info[4] == "P", "PUT", "CALL")
  )
}


#' @description Option mark price and greek info.
#' @return Returns a `tibble`
#' @examples
#' binance_eapi_mark(pair = "BTC-230224-24000-P")

binance_eapi_mark <- function(pair = "BTC-230224-24000-P"){

  url <- "https://eapi.binance.com"

  response <- NULL

  api_path <- c("eapi", "v1", "mark" )
  api_query <- list(symbol = pair)

  response <- Api(url = url, path = api_path, query = api_query)

  response <- dplyr::as_tibble(response)

  response <- dplyr::mutate(response,
                            Date = Sys.time(),
                            markPrice = as.numeric(markPrice),
                            bidIV = as.numeric(bidIV),
                            askIV = as.numeric(askIV),
                            delta = as.numeric(delta),
                            theta = as.numeric(theta),
                            gamma = as.numeric(gamma),
                            vega = as.numeric(vega),
                            highPriceLimit = as.numeric(highPriceLimit),
                            lowPriceLimit = as.numeric(lowPriceLimit)
                            )

  response <- dplyr::select(response, Date, Pair = "symbol", Price = "markPrice",
                            BID = "bidIV", ASK = "askIV", Mark = "markIV",
                            Delta = "delta", Theta = "theta", Gamma = "gamma", Vega = "vega",
                            highPriceLimit, lowPriceLimit)


  attr(response, "IpWgt") <- 5
  attr(response, "Api") <- "binance_eapi"
  return(response)

}


#' @description Get spot index price for option underlying.
#' @return Returns a `tibble`
#' @examples
#' binance_eapi_index(pair = "BTCUSDT")
#' binance_eapi_index("ETHUSDT")

binance_eapi_index <- function(pair = "BTCUSDT"){

  url <- "https://eapi.binance.com"

  response <- NULL

  pair_name <- toupper(pair)

  api_path  <- c("eapi", "v1", "index" )
  api_query <- list(underlying = pair)

  response <- Api(url = url, path = api_path, query = api_query)

  if(!purrr::is_empty(response)){

    response <- dplyr::as_tibble(response)
    response <- dplyr::mutate(response,
                              time = as.POSIXct(time/1000, origin = "1970-01-01"),
                              indexPrice = as.numeric(indexPrice),
                              indexPair = pair_name)

    response <- dplyr::select(response,
                              Date = "time",
                              Underlying = "indexPair",
                              Price = "indexPrice")

  }

  attr(response, "IpWgt") <- 1
  attr(response, "Api") <- "binance_eapi"
  return(response)

}


#' @description Get historical exercise records.
#' @return Returns a `tibble`
#' @examples
#' binance_eapi_exerciseHistory(pair = "BTCUSDT", from = NULL, to = "2023-02-06")
#' binance_eapi_exerciseHistory(pair = "BTCUSDT", from = NULL, to = "2023-01-20")

binance_eapi_exerciseHistory <- function(pair = "BTCUSDT", from = NULL, to = NULL){

  url <- "https://eapi.binance.com"

  response <- list()

  # Mandatory Arguments
  if(is.null(pair)){
    return(NULL)
  }

  # Pair, Path and Interval
  pair_name <- toupper(pair)
  api_path  <- c("eapi", "v1", "exerciseHistory" )

  # Start Date
  if(is.null(from)){
    from <- Sys.time() - lubridate::days(30)
  } else {
    from <- as.POSIXct(from)
  }

  # End Date
  if(is.null(to)){
    to <- Sys.time()
  } else {
    to <- as.POSIXct(to)
  }

  response  <- NULL
  startTime <- paste0(trunc(as.integer(from)), "000")
  endTime   <- paste0(trunc(as.integer(to)), "000")

  api_query <- list(underlying = pair_name, startTime = startTime, endTime = endTime, limit = 100)

  response <- Api(url = url, path = api_path, query = api_query)
  response <- dplyr::as_tibble(response)

  if(!purrr::is_empty(response)){

    response <- dplyr::as_tibble(response)
    response <- dplyr::mutate(response,
                              expiryDate = as.POSIXct(expiryDate/1000, origin = "1970-01-01"),
                              expiryDate = as.Date(expiryDate),
                              strikePrice = as.numeric(strikePrice),
                              Option = NA_character_,
                              realStrikePrice = as.numeric(realStrikePrice),
                              strikeResult = case_when(
                                strikeResult == "REALISTIC_VALUE_STRICKEN" ~ "Exercised",
                                strikeResult == "EXTRINSIC_VALUE_EXPIRED" ~  "Expired OTM",
                              ))

    response$Option <- map_chr(response$symbol, ~ifelse(strsplit(.x, "-")[[1]][4] == "P", "PUT", "CALL"))

    response <- dplyr::select(response,
                              Expiry = "expiryDate", Option,
                              Pair = "symbol", Strike = "strikePrice",
                              realStrike = "realStrikePrice",
                              Result = "strikeResult")

    dplyr::arrange(response, Expiry)

  }

  # Attributes
  attr(response, "IpWgt") <- 3
  attr(response, "Api") <- "binance_eapi"

  return(response)

}




