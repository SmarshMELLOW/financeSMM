#' Make different types of returns
#'
#' This function makes weekly and monthly returns for a single ticker. It calls the function ret_type.
#' @param data_df data frame with ticker and date variables
#' @keywords returns
#' @export
#' @examples
#' ret_compare()

ret_compare <- function( data_df ) {

  names( data_df ) <- c("prx", "date_id")

  # first make daily (252 trading days)
  #daily.df <- ret_type( data_df, N.lag = 1, P = 252, "daily")

  weekly.df <- ret_type( data_df, N.lag = 5, P = 50.4, "weekly")

  monthly.df <- ret_type( data_df, N.lag = 21, P = 12, "monthly")

  out.df <- bind_rows( weekly.df, monthly.df )

  return( out.df )
}

