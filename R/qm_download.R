#' Scrape Data using quantmod
#'
#' This function scrapes stock closing price data from google using quantmod. The data span 2007-present.
#' @param tic the ticker of the stock to download
#' @keywords quantmod finance
#' @import quantmod
#' @import magrittr
#' @import dplyr
#' @import lubridate
#' @export
#' @examples
#' qm_download()
#'
qm_download <- function( tic ) {

  tic.obj <- getSymbols(Symbols = tic, src='google', auto.assign = F)

  tic.obj %<>% as.data.frame( ) %>%
    mutate( date_id = as_date( ymd( row.names( . ) ) ) )

  names( tic.obj ) <- gsub( paste0(tic, "."), "", names( tic.obj ) )

  tic.obj %<>% select( Close, date_id )

  names( tic.obj ) <- c(tic, "date_id" )
  #tic.df %<>%
  #  mutate( date_id = as_date( ymd( row.names( tic.df ) ) ) )

  return( tic.obj )
}

