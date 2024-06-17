#' @export
#'
#' @rdname stat_sma
#' @name stat_sma

stat_sma <- function(mapping = NULL, data = NULL, 
                     position = "identity", linejoin = "mitre",
                     ..., na.rm = TRUE, show.legend = NA, n = 10, inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = StatSMA, 
    geom = GeomIndicator, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, n = n ,...)
  ) 
}

# Compute heikin-shi candles 
StatSMA <- ggproto("StatSMA", Stat,
                   required_aes = c("x", "y"),
                   default_aes = ggplot2::aes(
                     linewidth = .5,
                     size = 2,
                     n = 10,
                     linetype = 1,
                     fill = NA,
                     alpha = NA
                   ),
                   
                   setup_params = function(data, params) {
                     return(params)
                   },
                   compute_group = function(data, scales, n) {
                     
                     data$y <- TTR::SMA(data$y, n = n)
                     
                     return(data)
                   }  
)

#' @export
#'
#' @rdname stat_ema
#' @name stat_ema

stat_ema <- function(mapping = NULL, data = NULL, 
                     position = "identity", ..., na.rm = TRUE, 
                     show.legend = NA, n = 10, inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = StatEMA, 
    geom = GeomIndicator, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, n = n,...)
  ) 
}

# Compute heikin-shi candles 
StatEMA <- ggproto("StatEMA", Stat,
                   required_aes = c("x", "y"),
                   default_aes = ggplot2::aes(
                     linewidth = .5,
                     size = 2,
                     n = 10,
                     linetype = 1,
                     fill = NA,
                     alpha = NA
                   ),
                   setup_params = function(data, params) {
                     return(params)
                   },
                   compute_group = function(data, scales, n) {
                     
                     data$y <- TTR::EMA(data$y, n = n)
                     
                     return(data)
                   }  
)


#' @export
#'
#' @rdname stat_vwap
#' @name stat_vwap

stat_vwap <- function(mapping = NULL, data = NULL, 
                      position = "identity", linejoin = "mitre",
                      ..., na.rm = TRUE, show.legend = NA, n = 10, inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = StatVwap, 
    geom = GeomIndicator, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, n = n,...)
  ) 
}

# Compute heikin-shi candles 
StatVWAP <- ggproto("StatVwap", Stat,
                   required_aes = c("x", "y", "volume"),
                   default_aes = ggplot2::aes(
                     linewidth = .5,
                     size = 2,
                     n = 10,
                     linetype = 1,
                     fill = NA,
                     alpha = NA
                   ),
                   setup_params = function(data, params) {
                     return(params)
                   },
                   compute_group = function(data, scales, n) {
                     
                     data$y <- TTR::VWAP(data$y, data$volume, n = n)
                     
                     return(data)
                   }  
)

#' @export
#'
#' @rdname geom_indicator
#' @name geom_indicator

geom_indicator <- function(mapping = NULL, data = NULL, 
                           stat = "identity", position = "identity", ...,  
                           na.rm = TRUE, show.legend = NA, n = 10, inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    geom = GeomIndicator, 
    stat = stat, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, n = n, ...)
  ) 
}

# Compute heikin-shi candles 
GeomIndicator <- ggproto("GeomIndicator", GeomLine,
                         required_aes = c("x", "y"),
                         draw_panel = function(data, panel_params, coord, ...) {
                           
                           # Return all the components
                           grid::gList(
                             ggplot2::GeomLine$draw_panel(data, panel_params, coord, ...)
                           )
                         }
) 


#' @export
#'
#' @rdname geom_bbands
#' @name geom_bbands

geom_bbands <- function(mapping = NULL, data = NULL, 
                        stat = "bbands", position = "identity", ..., 
                        col_up = "green", col_dw = "red", col_av = "blue",
                        na.rm = TRUE, show.legend = NA, n = 10, inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    geom = GeomBbands, 
    stat = stat, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, 
                  n = n, 
                  col_up = col_up,
                  col_dw = col_dw,
                  col_av = col_av,
                  ...)
  ) 
}

#' @export
#'
#' @rdname stat_bbands
#' @name stat_bbands
stat_bbands <- function(mapping = NULL, data = NULL, 
                        position = "identity", ..., 
                        col_up = "green", col_dw = "red", col_av = "blue",
                        na.rm = TRUE, show.legend = NA, n = 10, inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = StatBbands, 
    geom = GeomBbands, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, 
                  n = n,
                  col_up = col_up,
                  col_dw = col_dw,
                  col_av = col_av,
                  ...)
  ) 
}

# Compute heikin-shi candles 
GeomBbands <- ggproto("GeomBbands", GeomLine,
                      required_aes = c("x", "y"),
                      draw_panel = function(data, panel_params, coord, ...) {
                        
                        data_up <- transform(data, y = y_up, colour = col_up)
                        data_dw <- transform(data, y = y_dw, colour = col_dw)
                        data_av <- transform(data, colour = col_av)
                        
                        # Return all the components
                        grid::gList(
                          ggplot2::GeomLine$draw_panel(data_av, panel_params, coord, ...),
                          ggplot2::GeomLine$draw_panel(data_up, panel_params, coord, ...),
                          ggplot2::GeomLine$draw_panel(data_dw, panel_params, coord, ...)
                        )
                      }
)


# Compute heikin-shi candles 
StatBbands <- ggproto("StatBbands", Stat,
                      required_aes = c("x", "y"),
                      default_aes = ggplot2::aes(
                        n = 10,
                        linewidth = 1, 
                        high = NULL, 
                        low = NULL,
                        col_up = "green", 
                        col_dw = "red", 
                        col_av = "blue"
                      ),
                      compute_group = function(data, scales, n, col_up, col_dw, col_av) {
                        
                        if (!is.null(data$high) & !is.null(data$low)) {
                          hlc <- tibble(close = data$y, high = data$high, low = data$low)
                        } else {
                          hlc <- tibble(close = data$y)
                        }

                        bbands <- TTR::BBands(hlc, n = n)
                        transform(data, 
                                  y_dw = bbands[,1],
                                  col_dw = col_dw, 
                                  y = bbands[,2], 
                                  col_av = col_av, 
                                  y_up = bbands[,3],
                                  col_up = col_up)
                      }  
) 


