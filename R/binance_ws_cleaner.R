# ----- Cleaner for the different response -----
binance_ws_cleaner <- function(data){

  UseMethod("binance_ws_cleaner")

}

binance_ws_cleaner.aggTrade <- function(data){

  if(purrr::is_empty(data)){
    return(dplyr::tibble())
  }

  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output,
                          Date = "E", aggId = "a", firstId = "f", lastId = "l",
                          Pair = "s", Price = "p", Qty = "q", Side = "m")

  output <- dplyr::mutate(output,
                          Date  = as.POSIXct(Date/1000, origin = "1970-01-01"),
                          Price = as.numeric(Price),
                          Qty   = as.numeric(Qty),
                          Side  = ifelse(Side, "SELL", "BUY")
                          )

  return(output)
}

binance_ws_cleaner.trade <- function(data){

  if(purrr::is_empty(data)){
    return(dplyr::tibble())
  }

  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output, Date = "T", Id = "t", Buy_Id = "b", Sell_Id = "a", Pair = "s", Price = "p", Qty = "q", Side = "m")
  output <- dplyr::mutate(output,
                          Date = as.POSIXct(Date/1000, origin = "1970-01-01"),
                          Price = as.numeric(Price),
                          Qty = as.numeric(Qty),
                          Side = ifelse(Side, "SELL", "BUY")
                          )

  return(output)

}

binance_ws_cleaner.miniTicker <- function(data){

  if(purrr::is_empty(data)){
    return(data)
  }

  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output, Date = "E", Pair = "s", Close = "c", Open = "o", High = "h", Low = "l", Volume = "v", Qty = "q")
  output <- dplyr::mutate(output,
                          Date = as.POSIXct(Date/1000, origin = "1970-01-01"),
                          Close = as.double(Close),
                          Open = as.double(Open),
                          High = as.double(High),
                          Low = as.double(Low),
                          Volume = as.double(Volume),
                          Qty = as.double(Qty)
                          )

  return(output)
}

binance_ws_cleaner.ticker <- function(data){

  if(purrr::is_empty(data)){
    return(data)
  }

  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output,
                          Date = "E", Pair = "s", Price_Change = "p", Price_Change_Perc = "P", Weighted_Price = "w",
                          Last_Qty = "Q", BID = "b", BID_Qty = "B", ASK = "a", ASK_Qty = "A",
                          Open = "o", Close = "c", High = "h", Low = "l", Volume = "v", Qty = "q", Trades = "n")

  output <- dplyr::mutate(output,
                          Date = as.POSIXct(Date/1000, origin = "1970-01-01"),
                          Price_Change = as.double(Price_Change),
                          Price_Change_Perc = as.double(Price_Change_Perc),
                          Weighted_Price = as.double(Weighted_Price),
                          Last_Qty = as.double(Last_Qty),
                          BID = as.double(BID),
                          BID_Qty = as.double(BID_Qty),
                          ASK = as.double(ASK),
                          ASK_Qty = as.double(ASK_Qty),

                          Close = as.double(Close),
                          Open = as.double(Open),
                          High = as.double(High),
                          Low = as.double(Low),
                          Volume = as.double(Volume),
                          Qty = as.double(Qty),
                          Trades = as.integer(Trades)
  )

  return(output)

}

binance_ws_cleaner.bookTicker <- function(data){

  if(purrr::is_empty(data)){
    return(data)
  }

  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output, Date = "u", Pair = "s", BID = "b", BID_Qty = "B", ASK = "a", ASK_Qty = "A")

  output <- dplyr::mutate(output,
                          Date = Sys.time(),
                          BID = as.double(BID),
                          BID_Qty = as.double(BID_Qty),
                          ASK = as.double(ASK),
                          ASK_Qty = as.double(ASK_Qty)
  )
  return(output)
}

binance_ws_cleaner.kline <- function(data){

  output <- dplyr::bind_rows(data$k)

  output <- dplyr::select(output,
                          Date = "t", dateClose = "T", Pair = "s",
                          Open = "o", Close = "c", High = "h", Low = "l", Trades = "n", isClosed = "x",
                          Volume = "v", volumeQuote = "q", takerBuy = "V", takerBuyQuote = "Q" )

  output <- dplyr::mutate(output,
                          Date = as.POSIXct(Date/1000, origin = "1970-01-01"),
                          dateClose = as.POSIXct(dateClose/1000, origin = "1970-01-01"),
                          Close = as.double(Close),
                          Open = as.double(Open),
                          High = as.double(High),
                          Low = as.double(Low),
                          Trades = as.integer(Trades),
                          Volume = as.double(Volume),
                          volumeQuote = as.double(volumeQuote),
                          takerBuy = as.double(takerBuy),
                          takerBuyQuote = as.double(takerBuyQuote)
  )

  return(output)

}

binance_ws_cleaner.continuousKline <- function(data){

  output <- dplyr::bind_rows(data$k)
  output <- dplyr::mutate(output, pair = data$ps, contract = data$ct)

  output <- dplyr::select(output,
                          Date = "t", dateClose = "T", Pair = "pair", contractType = "contract",
                          Open = "o", Close = "c", High = "h", Low = "l", Trades = "n", isClosed = "x",
                          Volume = "v", volumeQuote = "q", takerBuy = "V", takerBuyQuote = "Q" )

  output <- dplyr::mutate(output,
                          Date = as.POSIXct(Date/1000, origin = "1970-01-01"),
                          dateClose = as.POSIXct(dateClose/1000, origin = "1970-01-01"),
                          Close = as.double(Close),
                          Open = as.double(Open),
                          High = as.double(High),
                          Low = as.double(Low),
                          Trades = as.integer(Trades),
                          Volume = as.double(Volume),
                          volumeQuote = as.double(volumeQuote),
                          takerBuy = as.double(takerBuy),
                          takerBuyQuote = as.double(takerBuyQuote)
  )

  return(output)

}

binance_ws_cleaner.markPrice <- function(data){

  output <- dplyr::bind_rows(data)
  output <- dplyr::mutate(output, Date = Sys.time())
  output <- dplyr::select(output, Date, nextFundingDate = "T", Pair = "s", markPrice = "p", indexPrice = "i", estSettle = "P", fundingRate = "r")

  output <- dplyr::mutate(output,
                          markPrice = as.numeric(markPrice),
                          indexPrice = as.numeric(indexPrice),
                          estSettle = as.numeric(estSettle),
                          fundingRate = as.numeric(fundingRate),
                          nextFundingDate = as.numeric(nextFundingDate),
                          nextFundingDate = as.POSIXct(nextFundingDate/1000, origin = "1970-01-01")
  )

  return(output)
}

binance_ws_cleaner.forceOrder <- function(data){

  output <- dplyr::bind_rows(data$o)

  return(output)

}

binance_ws_cleaner.depth <- function(data){

  # Creazione dataframe con informazioni (Data e Pair)
  output <- dplyr::bind_rows(data[c(2,3,4,5)])
  output <- dplyr::select(output, Date = "E", Pair = "s")

  # Dataset Prezzi BID
  colnames(data$b) <- c("Price", "Qty")
  df_BID <- dplyr::as_tibble(data$b)
  df_BID <- dplyr::mutate_all(df_BID, as.numeric)
  df_BID <- dplyr::bind_cols(output, df_BID, Side = "BID" )

  # Dataset Prezzi BID
  colnames(data$a) <- c("Price", "Qty")
  df_ASK <- dplyr::as_tibble(data$a)
  df_ASK <- dplyr::mutate_all(df_ASK, as.numeric)
  df_ASK <- dplyr::bind_cols(output, df_ASK, Side = "ASK" )

  output <- dplyr::bind_rows(df_ASK, df_BID)
  output <- dplyr::mutate(output,
                          Date = as.POSIXct(Date/1000, origin = "1970-01-01"),
                          Side = factor(Side, levels = c("ASK", "BID"), ordered = FALSE)
  )

  output <- dplyr::group_by(output, Date, Pair )
  output <- tidyr::nest(output, order_book = c("Price", "Qty", "Side"))
  output <- dplyr::ungroup(output)
  output

}

# ----- Structure to avoid useless data -----

# Structure the Kline to avoid to add unclosed canldes
binance_ws_kline <- function(data_before, data_after){

  if(purrr::is_empty(data_before)){
    return(data_after)
  }

  isClosed <- data_before[1,]$isClosed

  if(isClosed){
    merge_data <- dplyr::bind_rows(data_after, data_before)
  } else {
    merge_data <- dplyr::bind_rows(data_after, data_before[-1,])
  }

  return(merge_data)
}

# Structure an Order Book updating the levels
binance_ws_orderbook <- function(data_before = NULL, data_after){

  if(purrr::is_empty(data_before)){
    return(data_after)
  }

  df1 <- data_before
  df2 <- data_after

  last_update <- data_after$Date[1]

  df1 <- df1 %>%
    tidyr::unnest(cols = c("order_book")) %>%
    dplyr::group_by(Pair, Side, Price) %>%
    dplyr::summarise(Qty = sum(Qty), .groups = "rowwise") %>%
    dplyr::ungroup()

  df2 <- df2 %>%
    tidyr::unnest(cols = c("order_book")) %>%
    dplyr::group_by(Pair, Side, Price) %>%
    dplyr::summarise(Qty = sum(Qty), .groups = "rowwise") %>%
    dplyr::ungroup()

  df_out <- dplyr::full_join(df1, df2, by = c("Pair", "Side", "Price")) %>%
    dplyr::mutate(Qty = dplyr::case_when(
      is.na(Qty.y) ~ Qty.x,
      TRUE ~ Qty.y
    )) %>%
    dplyr::mutate(Date = last_update)

  df_out <- dplyr::select(df_out, Date, Pair, Side, Price, Qty)

  df_out %>%
    dplyr::group_by(Date, Pair ) %>%
    dplyr::filter(Qty > 0) %>%
    tidyr::nest(order_book = c("Price", "Side", "Qty")) %>%
    dplyr::ungroup()
}

