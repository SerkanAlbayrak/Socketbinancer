#' Candlestick plot 
#'
#' Candlestick plot for all time frames within the ggplot2 framework
#' 
#' @inheritParams ggplot2::geom_rect
#' @param stat The statistical transformation to use on the data for this layer, either as a 
#' ggproto Geom subclass or as a string naming the stat stripped of the stat_ prefix 
#' (e.g. "candle" rather than "stat_candle" or "heikin_ashi" rather than "stat_heikin_ashi")
#' @param col_up Character, color of the candle when open price is greater than close price. 
#' @param col_dw Character, color of the candle when open price is lower than close price. 
#' @param bargap Numeric, positive number to regulate the distance between candles. 
#' Increasing the `"bargap"` reduce the distance between candles. Default is `6`. 
#'
#' @usage 
#' geom_candlechart(mapping = NULL, 
#'                  data = NULL, 
#'                  stat = "candle", 
#'                  position = "identity", 
#'                  linejoin = "mitre",..., 
#'                  na.rm = FALSE, 
#'                  show.legend = NA, 
#'                  bargap = 6, 
#'                  method = "candle",
#'                  col_up = "green", 
#'                  col_dw = "red", 
#'                  inherit.aes = TRUE)
#'                  
#' @examples 
#' df <- binance_klines(pair = "BTCUSDT", interval = "1h)
#' 
#' require(ggplot2)
#' ggplot(df)+
#' geom_candlestick(aes(x = date, date_close = date_close, 
#'                      open = open, close = close, 
#'                      high = high, low = low), stat = "candle")
#'                      
#' ggplot(df)+
#' geom_candlestick(aes(x = date, date_close = date_close,
#'                      open = open, close = close, 
#'                      high = high, low = low), stat = "candle")
#'                      
#' @export
#'
#' @rdname geom_candlechart
#' @name geom_candlechart

geom_candlechart <- function(mapping = NULL, data = NULL, 
                             stat = "candle", position = "identity", linejoin = "mitre",
                             ..., na.rm = FALSE, show.legend = NA, 
                             bargap = 6, col_up = "green", col_dw = "red", ts = "1d", inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = GeomCandlechart, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, 
                  bargap = bargap, 
                  col_up = col_up,
                  col_dw = col_dw,
                  ts = ts,
                  ...)
  ) 
}

# GeomCandleStick is geom_candlestick (candlestick) 
GeomCandlechart <- ggplot2::ggproto("GeomCandlechart", Geom,
                                    required_aes = c("x", "open", "close", "low", "high"),
                                    default_aes = ggplot2::aes(
                                      colour = "black",
                                      col_up = "green",
                                      col_dw = "red",
                                      ts = "1d",
                                      linewidth = .5,
                                      size = 2,
                                      bargap = 6,
                                      linetype = 1,
                                      shape = 19,
                                      fill = NA,
                                      alpha = NA,
                                      stroke = 1
                                    ),
                                    draw_panel = function(data, panel_params, coord, ...) {
                                      
                                      # x            (date)
                                      # xmax         (date_close)
                                      # close price  (y_close)
                                      # low price    (y_low)
                                      # high price   (y_high)
                                      # open price   (y_open)
                                      
                                      candle_body <- dplyr::mutate(data,
                                                                   fill = mycol) 
                                      
                                      candle_high <- dplyr::mutate(data,
                                                                   x = (xmin + xmax)/2, 
                                                                   xend = (xmin + xmax)/2,
                                                                   y = ifelse(move == "up", ymax, ymin),
                                                                   yend = y_high)
                                      
                                      candle_low <- dplyr::mutate(data,
                                                                  x = (xmin + xmax)/2, 
                                                                  xend = (xmin + xmax)/2,
                                                                  y = ifelse(move == "up", ymin, ymax), 
                                                                  yend = y_low)
                                      
                                      # Return all the components
                                      grid::gList(
                                        ggplot2::GeomRect$draw_panel(candle_body, panel_params, coord, ...),
                                        ggplot2::GeomSegment$draw_panel(candle_high, panel_params, coord, ...),
                                        ggplot2::GeomSegment$draw_panel(candle_low, panel_params, coord, ...)
                                      )
                                    }
                                  ) 

#' @export
#'
#' @rdname geom_ohlc
#' @name geom_ohlc

geom_candlebar <- function(mapping = NULL, data = NULL, 
                           stat = "candle", position = "identity", ..., na.rm = FALSE, show.legend = NA, 
                           bargap = 600, col_up = "green", col_dw = "red", ts = "1d", inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = GeomCandlebar, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, 
                  bargap = bargap, 
                  col_up = col_up,
                  col_dw = col_dw,
                  ts = ts,
                  ...)
  ) 
}

# GeomCandleStick is geom_candlestick (candlestick) 
GeomCandlebar <- ggplot2::ggproto("GeomCandlebar", Geom,
                                  required_aes = c("x", "open", "close", "low", "high"),
                                  default_aes = ggplot2::aes(
                                    col_up = "green",
                                    col_dw = "red",
                                    ts = "1d",
                                    linewidth = 0.5,
                                    size = 2,
                                    bargap = 600,
                                    linetype = 1,
                                    shape = 19,
                                    fill = NA,
                                    alpha = NA,
                                    stroke = 1
                                  ),
                                  draw_panel = function(data, panel_params, coord, ...) {
                                    
                                    # x (date)
                                    # xmax (date_close)
                                    # close price  (y_close)
                                    # low price (y_low)
                                    # high price (y_high)
                                    # open price (y_open)
                                    
                                    bar_body <- dplyr::mutate(data,
                                                              x = (xmin + xmax)/2, 
                                                              xend = (xmin + xmax)/2, 
                                                              y = y_low, 
                                                              colour = mycol,
                                                              yend = y_high,
                                                              colour = mycol) 
                                    
                                    bar_open <- dplyr::mutate(data,
                                                              x = (xmin + xmax)/2, 
                                                              xend = xmin,
                                                              y = ymin, 
                                                              colour = mycol,
                                                              yend = ymin) 
                                    
                                    bar_close <- dplyr::mutate(data,
                                                               x = (xmin + xmax)/2, 
                                                               xend = xmax,
                                                               y = ymax, 
                                                               colour = mycol,
                                                               yend = ymax) 
                                    
                                    # Return all the components
                                    grid::gList(
                                      ggplot2::GeomSegment$draw_panel(bar_body, panel_params, coord, ...),
                                      ggplot2::GeomSegment$draw_panel(bar_open, panel_params, coord, ...),
                                      ggplot2::GeomSegment$draw_panel(bar_close, panel_params, coord, ...)
                                    )
                                  }
                                ) 

stat_heikin_ashi <- function(mapping = NULL, data = NULL, 
                             geom = "candlechart", position = "identity", linejoin = "mitre",
                             ..., na.rm = TRUE, show.legend = NA, 
                             bargap = 6, col_up = "green", col_dw = "red", ts = "1d",
                             inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = StatHeikinAshi, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, 
                  bargap = bargap, 
                  col_up = col_up,
                  col_dw = col_dw,
                  ts = ts,
                  ...)
  ) 
}

# Compute heikin-shi candles 
StatHeikinAshi <- ggproto("StatHeikinAshi", Stat,
                          required_aes = c("x", "open", "close", "low", "high"),
                          
                          setup_params = function(data, params) {
                            return(params)
                          },
                          compute_group = function(data, scales, bargap, col_up, col_dw, ts) {
                            
                            data <- 
                              dplyr::mutate(data,
                                            xmin = as.numeric(x), 
                                            xmax = as.numeric(x + match_duration(ts)),
                                            dx = (xmax - xmin)/bargap,
                                            xmax = xmax - dx,
                                            ymax = (open + low + close + high)/4, # close price 
                                            ymin = open, # open price
                                            y_high = high, # high price 
                                            y_low = low, # low price
                                            move = ifelse(ymin < ymax, "up", "dw"),
                                            mycol = ifelse(move == "up", col_up, col_dw))
                            
                            for(i in 2:nrow(data)){
                              data$ymin[i] <- (data$ymin[i-1] + data$ymax[i-1])/2
                              data$y_low[i] <- min(c(data$ymin[i], data$ymax[i], data$y_low[i]))
                              data$y_high[i] <- max(c(data$ymin[i], data$ymax[i], data$y_low[i]))
                              data$ymax[i] <- (data$ymin[i] + data$ymax[i] + data$y_low[i] + data$y_high[i])/4
                              data$move[i] <- ifelse(data$ymin[i] < data$ymax[i], "up", "dw")
                              data$mycol[i] <- ifelse(data$move[i] == "up", col_up, col_dw)
                            }
                            return(data)
                          }  
)


stat_candle <- function(mapping = NULL, data = NULL, 
                             geom = "candlechart", position = "identity", linejoin = "mitre",
                             ..., na.rm = FALSE, show.legend = NA, 
                             bargap = 6, col_up = "green", col_dw = "red", ts = "1d", 
                             inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = StatCandle, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, 
                  bargap = bargap, 
                  col_up = col_up,
                  col_dw = col_dw,
                  ts = ts, 
                  ...)
  ) 
}

# Compute "classic" candles 
StatCandle <- ggproto("StatCandle", Stat,
                      required_aes = c("x", "open", "close", "low", "high"),
                      
                      setup_params = function(data, params) {
                        return(params)
                      },
                      compute_group = function(data, scales, bargap, col_up, col_dw, ts) {
                        
                        data <- 
                          dplyr::mutate(data,
                                        xmin = as.numeric(x), 
                                        xmax = as.numeric(x + match_duration(ts)),
                                        dx = (xmax - xmin)/bargap,
                                        xmax = xmax - dx,
                                        ymin = open, # open price
                                        ymax = close, # close price 
                                        y_high = high, # high price 
                                        y_low = low, # low price
                                        move = ifelse(ymin < ymax, "up", "dw"),
                                        mycol = ifelse(move == "up", col_up, col_dw))
                        
                        return(data)
                      }  
                    )

match_duration <- function(x){
  
  interval <- stringr::str_extract(x, "[0-9][0-9]|[0-9]")
  units <- stringr::str_extract(x, "[a-z]|[A-Z]")
  
  av_units <- c(seconds = "s", minutes = "m", hours = "h", days = "d", weeks = "W", months = "M")
  units <- match.arg(units, choices = av_units)
  
  lubridate::duration(as.numeric(interval), units = names(units))
}


