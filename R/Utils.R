#'@name WeigthIP
#'@rdname WeigthIP
#'@description Extract the attribute IpWgt from an object
#'
#' @export

WeigthIP <- function(object){
  attr(object, "IpWgt")
}

#'@name bind_by_col
#'@rdname bind_by_col
#'@description bind_rows between two dataframes checking that the elements in "col" of the
#' data_after are NOT in the elements in "col" in data_before
#' useful for observation data that comes in sequences such as the id of the trades
#'
#' @export

bind_by_col <- function(data_before, data_after, col = "Date", order = c("none", "desc", "asc")){

  if(missing(data_before) & missing(data_after)){
    warning("Both datasets are missing")
    return(NULL)
  }

  if(is.null(data_before) & is.null(data_after)){
    warning("Both datasets are NULL")
    return(NULL)
  }

  if(is.null(data_before) & !is.null(data_after)){
    warning("data_before dataset is missing")
    return(data_after)
  }

  if(!is.null(data_before) & is.null(data_after)){
    warning("data_after dataset is missing")
    return(data_before)
  }

  old_dates <- data_before[[col]]

  data_after_index <- which(!(data_after[[col]] %in% data_before[[col]]))

  data_final <- dplyr::bind_rows(data_after[data_after_index,], data_before)

  if(order == "desc"){
    data_final <- dplyr::arrange(data_final, dplyr::desc(col))
  } else if(order == "asc"){
    data_final <- dplyr::arrange(data_final, col)
  }

  return(data_final)

}

#'@name tbl_to_xts
#'@rdname tbl_to_xts
#'@description convert a `tibble` into an `xts` object
#'@order.by the name of the date column
#'
#' @export

tbl_to_xts <- function(x, order.by = "Date"){

  if(is.null(order.by)){
    msg <- paste0('"order.by" must be a vector containing the dates or \n
                  a the name of the column containing the dates.')
    stop(msg)
  }

  time_series <- x

  if(lubridate::is.Date(order.by)){

    date_index <- order.by

  } else if(is.character(order.by)){

    date_index <- time_series[[order.by]]

    rm_index <- which(colnames(time_series) == order.by)

    time_series <- time_series[,-c(rm_index)]

  }

  xts::xts(x = time_series, order.by = date_index)

}


#'@name xts_to_tbl
#'@rdname xts_to_tbl
#'@description convert an `xts` into an `tibble` object
#'@order.by the name of the date column
#'
#' @export

xts_to_tbl <- function(x, order.by = "Date"){

  if(is.null(order.by)){
    msg <- paste0('"order.by" is set to "Date"')
    order.by <- "Date"
    warning(msg)
  }

  time_series <- as_tibble(x)

  date_index <- xts::xcoredata(x)$index

  date_index <- as.POSIXct(date_index, origin = "1970-01-01")

  time_series <- bind_cols(Date = date_index, time_series)

  colnames(time_series) <- c(order.by, colnames(time_series)[-1])

  time_series
}


#'@name candleChart
#'@rdname candleChart
#'@description Candlestick Plot
#'
#' @export

candleChart <- function(data, from = NULL, to = NULL, title = NULL){

  if(missing(data)){
    return(NULL)
  }

  if(is.null(data)){
    return(ggplot())
  }

  if(is.null(title)){

    # Title of the plot from column "pair"
    plot_title <- paste0("OHLC Data (", data$Pair[1], ")")

  } else {
    plot_title <- title
  }

  df_plot <- data

  if(!is.null(from)){
    df_plot <- dplyr::filter(df_plot, Date >= as.POSIXct(from))
  }

  if(!is.null(to)){
    df_plot <- dplyr::filter(df_plot, Date <= as.POSIXct(to))
  }

  # Determine the movement
  df_plot <- dplyr::mutate(df_plot, movement = ifelse(Close >= Open, "Up", "Down"))

  # Distance between the candles
  alpha <- 0.8*(df_plot$Date[2] - df_plot$Date[1])

  plot_output <-  ggplot()+
    geom_segment(data = df_plot, aes(x = Date + alpha/2, xend = Date + alpha/2, y = Low, yend = High)) +
    geom_rect(data = df_plot, aes(xmin = Date, xmax = Date + alpha, ymin = Open, ymax = Close, fill = movement), color = "black")+
    scale_fill_manual(values = c(Up = "green", Down = "red")) +
    scale_color_manual(values = c(Up = "green", Down = "red")) +
    theme(
      axis.title  = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold",size = 10),
      axis.title.y = element_text(face = "bold", size = 10),

      plot.title  = element_text(face = "bold"),
      plot.subtitle = element_text(face = "italic"),
      plot.caption = element_text(face = "italic"),

      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white"),

      panel.grid.major.x = element_line(colour = "blue" , linetype="dotted", linewidth = 0.2),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(colour="red", linetype="dotted", linewidth = 0.2),
      panel.grid.minor.y = element_blank(),

      axis.text.x  = element_text(angle = 0, face = "bold", size = 7),
      axis.text.y = element_text(face = "bold", size = 7) ,

      legend.text = element_text(face = "italic", size = 10),
      legend.title = element_text(face = "bold"),
      legend.position = "None" ) +
    xlab("")+
    ylab("")+
    ggtitle(plot_title)

  time_frame <- lubridate::as.difftime(df_plot$Date[2] - df_plot$Date[1])
  time_frame <- attr(time_frame, "units")

  min_date <- min(df_plot$Date)
  max_date <- max(df_plot$Date)

  N <- nrow(df_plot)

  seq_breaks = seq.POSIXt(as.POSIXct(min_date), as.POSIXct(max_date), length.out = round(2*log(N)) )

  if(time_frame == "days"){

    plot_output <- plot_output +
      scale_x_datetime(breaks = seq_breaks, date_labels = "%b %d")

  } else {

    plot_output <- plot_output+
      scale_x_datetime(breaks = seq_breaks, date_labels = "%H:%M")
  }

  attr(plot_output, "orig_data")  = data
  attr(plot_output, "plot_data")  = df_plot
  attr(plot_output, "time_frame") = time_frame

  return(plot_output)
}

#'@name add_line
#'@rdname add_line
#'@description Add lines to the candlestick plot
#'
#' @export

add_line <- function(plt, level = 32000, color = "black", size = 1){

  plot_data <- attr(plt, "plot_data")

  line_data <- dplyr::tibble(x = min(plot_data$Date), xend = max(plot_data$Date),
                             y = level, yend = level)

  plt <- plt +
    geom_segment(data = line_data, aes(x = x, xend = xend, y = y, yend = yend), color = color, size = size)

  plt
}


#'@name OrderBook
#'@rdname OrderBook
#'@description create and/or structure an existent order book
#'
#' @export

OrderBook <- function(data = NULL, min_price = NULL, max_price = NULL, by = 100, levels = NULL, as.datatable = FALSE){

  # Default min_price
  if(is.null(min_price)){
    min_price = 22500
  }

  # Default max_price
  if(is.null(max_price)){
    max_price = 25500
  }

  # Default by
  if(is.null(by)){
    by = 100
  }

  # levels > by
  if(is.null(levels)){
    seq_price <- seq(min_price, max_price, by)
  } else {
    seq_price <- seq(min_price, max_price, (max_price-min_price)/(levels-1))
  }

  # Empty Order book
  order_book <- dplyr::tibble(Price = seq_price, BUY = 0, BID = 0, ASK = 0, SELL = 0, NET = 0)

  # Return Empty Order Book
  if(is.null(data)){
    order_book <- dplyr::arrange(order_book, desc(Price))
    return(order_book)
  }

  # Structure the Levels
  i = 1
  original_order_book <- data
  for(i in 1:nrow(order_book)){

    if(i == 1){
      index <- original_order_book$Price <= order_book[i,]$Price
    } else if( i == nrow(order_book)) {
      index <- original_order_book$Price > order_book[i,]$Price
    } else {
      index <- original_order_book$Price > order_book[i-1,]$Price & original_order_book$Price <= order_book[i,]$Price
    }

    new_data <- original_order_book[which(index),]

    # ASK
    order_book[i,]$ASK <- sum(dplyr::filter(new_data, Side == "ASK")$Qty, na.rm = TRUE)

    # BID
    order_book[i,]$BID = sum(dplyr::filter(new_data, Side == "BID")$Qty, na.rm = TRUE)

  }

  # Order the Book
  order_book <- dplyr::arrange(order_book, dplyr::desc(Price))

  # Display a Datatable
  if(as.datatable){

    order_book <- DT::datatable(
      order_book, options = list(
        lengthMenu = list(c(20, 50, -1), c('5', '15', 'All')),
        pageLength = 50,
        searching = FALSE,
        paging = FALSE,
        processing = FALSE
      )
    )%>%
      DT::formatRound(c("Price"), 0) %>%
      DT::formatRound(c("BID", "ASK", "Price"), 2) %>%
      DT::formatStyle("BID",
                  background = DT::styleColorBar(range(order_book$BID), 'green'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')%>%
      DT::formatStyle("ASK",
                  background = DT::styleColorBar(range(order_book$ASK), '#961E0C'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')%>%
      DT::formatStyle('BID',
                  backgroundColor = "#00E679", color = "white")%>%
      DT::formatStyle('ASK',
                  backgroundColor = "#FF5F47", color = "white")

  }

  return(order_book)

}

#'@name TradeBook
#'@rdname TradeBook
#'@description create and/or structure an existent trading book
#'
#' @export

TradeBook <- function(data = NULL, min_price = 22500, max_price = 25000, by = 100, levels = NULL){

  # Default min_price
  if(is.null(min_price)){
    min_price = 22500
  }

  # Default max_price
  if(is.null(max_price)){
    max_price = 25500
  }

  # Default by
  if(is.null(by)){
    by = 100
  }

  # levels > by
  if(is.null(levels)){
    seq_price <- seq(min_price, max_price, by)
  } else {
    seq_price <- seq(min_price, max_price, (max_price-min_price)/(levels-1))
  }

  # Empty Trade book
  trade_book <- dplyr::tibble(Price = seq_price, BUY = 0, SELL = 0, NET = 0)

  # Return Empty Order Book
  if(is.null(data)){
    trade_book <- dplyr::arrange(trade_book, desc(Price))
    return(trade_book)
  }

  original_trade_book <- data

  i = 1
  for(i in 1:nrow(trade_book)){

    if(i == 1){
      index <- original_trade_book$Price <= trade_book[i,]$Price
    } else if( i == nrow(trade_book)) {
      index <- original_trade_book$Price > trade_book[i,]$Price
    } else {
      index <- original_trade_book$Price > trade_book[i-1,]$Price & original_trade_book$Price <= trade_book[i,]$Price
    }

    new_data <- original_trade_book[which(index),]


    # BUY
    trade_book[i,]$BUY <- sum(dplyr::filter(new_data, Side == "BUY")$Qty, na.rm = TRUE)

    # SELL
    trade_book[i,]$SELL <- sum(dplyr::filter(new_data, Side == "SELL")$Qty, na.rm = TRUE)

    # NET
    trade_book[i,]$NET <- trade_book[i,]$BUY - trade_book[i,]$SELL
  }

  trade_book <- dplyr::arrange(trade_book, dplyr::desc(Price))

  return(trade_book)

}
