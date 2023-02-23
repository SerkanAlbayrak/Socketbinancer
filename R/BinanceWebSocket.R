#'@name BinanceWebSocket
#'@rdname BinanceWebSocket
#'@import websocket
#'@import tidyverse
#'@import R6
#'@export

BinanceWebSocket <- R6::R6Class("BinanceWebSocket",
                                public = list(

                                  initialize = function(){
                                    private$subscriptions <- list()
                                  },

                                  Subscribe = function(pair = "BTCUSDT", api = "Spot", subscription = "kline",
                                                       interval = "1m", update.speed = "1000", quiet = FALSE,
                                                       contractType = NULL){

                                    pair_name <- toupper(pair)

                                    if(api == "Spot"){
                                      new_ws <- binance_ws_api_socket(pair = pair_name, subscription = subscription, interval = interval, update.speed = update.speed, quiet = quiet)
                                    } else if(api == "Futures USD-M"){
                                      new_ws <- binance_ws_fapi_socket(pair = pair_name, subscription = subscription, interval = interval, update.speed = update.speed, quiet = quiet, contractType = contractType)
                                    } else {
                                      stop("API NOT implemented!")
                                    }

                                    new_ws$connect()

                                    if(!(subscription %in% c("kline", "continuousKline"))){
                                      interval <- NULL
                                    } else {
                                      interval <- paste0("_", interval)
                                    }


                                    if(pair_name %in% names(private$subscriptions)){
                                      private$subscriptions[[pair_name]][[paste0(subscription, interval)]] <- new_ws
                                    } else {
                                      private$subscriptions[[pair_name]] <- list()
                                      private$subscriptions[[pair_name]][[paste0(subscription, interval)]] <- new_ws
                                    }
                                  },

                                  Info = function(){

                                    pairs <- names(private$subscriptions)
                                    output <- list()

                                    if(length(pairs) < 1){
                                      return(tibble(Pair = character(),Subscriptions = character()))
                                    }

                                    for(i in 1:length(private$subscriptions)){

                                      output[[i]] <- dplyr::tibble(Pair = pairs[i],
                                                                   Subscriptions = names(private$subscriptions[[i]]))

                                    }
                                    dplyr::bind_rows(output)
                                  },

                                  Get = function(pair, subscription){

                                    pair_name = toupper(pair)

                                    data <- private$subscriptions[[pair_name]][[subscription]]

                                    data$.__enclos_env__$stream$data
                                  },

                                  Unsubscribe = function(pair = NULL, subscription = NULL){

                                    pair_name <- toupper(pair)
                                    if(!is.null(pair) & is.null(subscription)){

                                      data <- private$subscriptions[[pair_name]]

                                      for(i in 1:length(data)){
                                        data[[i]]$close()
                                      }

                                    } else if(!is.null(pair) & !is.null(subscription)){

                                      data <- private$subscriptions[[pair_name]][[subscription]]

                                      data$close()

                                    } else {

                                      data <- private$subscriptions

                                      for(i in 1:length(data)){
                                        for(j in 1:length(data[[i]])){
                                          data[[i]][[j]]$close()
                                        }
                                      }
                                    }

                                  }
                                ),
                                private = list(
                                  subscriptions = NULL
                                )
)

