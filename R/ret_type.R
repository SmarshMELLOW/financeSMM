#' Create stock returns and variance of a given duration
#'
#' This function takes a single stock and creates the return based on the number of lags and then returns
#'   the non-missing remaining data and their variances
#' @param data_df data frame
#' @param N.lag Number of lags
#' @param P the power needed to annualize those lags
#' @param l.name the name of the type of return e.g. weekly, monthly
#' @keywords return variance
#' @import magrittr
#' @import dplyr
#' @export
#' @examples
#' ret_type()

ret_type <- function( data_df, N.lag, P, l.name ) {

  out.df <- data_df %>%
    mutate( ret = prx / lag(prx, n=N.lag),
            ann_ret = ret^P,
            year_id = year( date_id ) ) %>%
    filter( is.na(ret) == FALSE ) %>%
    group_by( year_id ) %>%
    summarize( avg_ret = round( mean( ann_ret ), digits = 2),
               var_ret = round( var( ann_ret ), digits = 2),
               type = paste0("Annualized ", l.name) )

  return( out.df )

}
