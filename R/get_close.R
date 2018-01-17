#' Scrape daily close prices for a list of tickers
#'
#' This function scrapes stock closing price data from google using quantmod. The data span 2007-present.
#' @param tic_list a list of tickers to be scraped
#' @keywords quantmod finance
#' @import quantmod
#' @import magrittr
#' @import dplyr
#' @import lubridate
#' @export
#' @examples
#' get_close()
#'
get_close <- function( tic_list ) {

  list.len <- length( tic_list )  # number of tickers to be downloaded

  for( i in 1: list.len ) {
    tic <- tic_list[i]

    tic.data <- getSymbols(Symbols = tic, src='google', auto.assign = F) %>%
      as.data.frame( ) %>%
      mutate( date_id = as_date( ymd( row.names( . ) ) ) ) %>%
      select( ends_with("Close"), date_id )

    # sometimes the google data doesn't go very far back, so use yahoo
    if (min(tic.data$date_id) > ymd("2007-01-03") ) {
      print(paste0("use yahoo for ", tic) )
      tic.yahoo <- getSymbols(Symbols = substring( tic, regexpr(":", tic) + 1),
                              src='yahoo', auto.assign = F) %>%
        as.data.frame( ) %>%
        mutate( date_id = as_date( ymd( row.names( . ) ) ) ) %>%
        select( ends_with("Adjusted"), date_id )

      # replace existing data if yahoo goes further back
      if (min(tic.yahoo$date_id) < min(tic.data$date_id)) {
        tic.data <- tic.yahoo
      }
    }

    # I don't want the exchange in the returned ticker name
    tic_id <- substring( tic, regexpr(":", tic) + 1)
    names( tic.data ) <- c(tic_id, "date_id" )

    if( i == 1 ) {
      tic.out <- tic.data
    } else {
      tic.out <- merge( tic.out, tic.data, by = "date_id", all = TRUE)
    }
  }

  return( tic.out )
}

