#' Find the start of the data or filter stocks that start after a certain date.
#'
#' This function finds the minimum date for which all stocks in a df have
#'   return data and removes all dates prior to that date. When a start date
#'   is specified, all stocks for which the data starts after that date are
#'   removed.
#' @param df the dataframe of stock prices to be manipulated
#' @param start_dt character yyyy-mm-dd
#' @keywords finance
#' @import magrittr
#' @import dplyr
#' @import lubridate
#' @export
#' @examples
#' rm_NAret(IYE.df, start_dt = "2012-01-01")
#'
rm_NAret <- function( df, start_dt = NA ) {

  # no start date is specified
  if (is.na(start_dt) == TRUE){
    period_st <- ymd("2007-01-03")
    tic.list <- names(df)[names(df) != "date_id"]
    for (tic in tic.list) {
      tic.df <- df %>% select_("date_id", tic) %>%
        filter_( paste0("is.na(", tic, ") == FALSE" ) )
      if( min(tic.df$"date_id") > ymd("2007-01-03")) {
        print(paste0("The start date for ", tic, " is ", min(tic.df$date_id)))
        # update start date if greater
        if( min(tic.df$"date_id") > period_st) {
          period_st <- min(tic.df$"date_id")
        }
      }
    }
    out.df <- df %>% filter( date_id >= period_st)
  } else{
    out.df <- .force_start(df, start_dt)
  }

  return( out.df )
}

#--------------------------------------------------------------------
.force_start <- function( df, start_dt ) {
  out.df <- df
  tic.list <- names(df)[names(df) != "date_id"]
  for (tic in tic.list) {
    tic.df <- df %>% select_("date_id", tic) %>%
      filter_( paste0("is.na(", tic, ") == FALSE" ) )

    if( min(tic.df$"date_id") > start_dt) {
      print(paste0("Remove ", tic, ". Starts on ", min(tic.df$date_id)))
      # remove the stock from the df
      out.df %<>% select_(paste0("-",tic))
    }
  }

  out.df %<>% filter(date_id >= start_dt)
  return( out.df )
}

