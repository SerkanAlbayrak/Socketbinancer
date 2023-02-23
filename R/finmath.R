#'@name MicroPrice
#'@rdname MicroPrice
#'@description Compute the MicroPrice
#'
#' @export

MicroPrice <- function(data){

  ASK <- dplyr::filter(data, Side == "ASK")

  BID <- dplyr::filter(data, Side == "BID")

  ASK <- ASK[which.min(ASK$Price),]

  BID <- BID[which.max(BID$Price),]

  Pmicro <- (ASK$Price * ASK$Qty + BID$Price * BID$Qty)/(ASK$Qty + BID$Qty)

  return(Pmicro)

}
#'@name MicroPrice
#'@rdname MicroPrice
#'@description Compute the cumulated Sign
#'
#' @export

cumDeltaSgn <- function(data){

  data <- data %>%
    dplyr::mutate(Date = Date - lubridate::second(Date)) %>%
    dplyr::group_by(Date, Side) %>%
    dplyr::summarise(n = n(), .groups = "rowwise") %>%
    tidyr::spread(Side, n) %>%
    dplyr::mutate(Sgn = BUY - SELL,
           Prop_Sell = ifelse(Sgn < 0, -Sgn/SELL*100, 0),
           Prop_Buy  = ifelse(Sgn > 0, Sgn/BUY*100, 0)
    ) %>%
    dplyr::arrange(dplyr::desc(Date))

  data$Cum <- cumsum(data$Sgn)

  return(data)

}
