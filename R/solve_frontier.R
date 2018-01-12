#' calculate the efficient frontier
#'
#' In more detail ... This function
#' @param data_df the dataframe to be manipulated
#' @param date_var the name of the date variable in quotes
#' @param min_ret the minimum return
#' @param max_ret the maximum return
#' @param size_out the number of obs in the returned df
#' @keywords finance markowitz bullet
#' @import magrittr
#' @import dplyr
#' @export
#' @examples
#' solve_frontier()
#'
# function to make the frontier
solve_frontier <- function( data_df, date_var, min_ret = 1.0001, max_ret = 1.01,
                            size_out = 100) {

  # need the vcov matrix to calculate the ptf var, the stock weights and N stocks
  # make the vcov matrix
  S <- data_df %>%
    select_( paste0("-",date_var) ) %>%
    var( )

  # get the number of stocks
  dim_ptf <- ncol( data_df ) - 1

  # sequence of returns for the bullet
  bullet_ret <- seq( from = min_ret, to = max_ret, length.out = size_out )

  #baseline to start the df
  # calculate the efficient weights
  eff_weights <- ptf_wts( data_df, date_var, min_ret)
  pf_var <- as.numeric( t( eff_weights ) %*% S %*% eff_weights )

  bullet.df <- data_frame( E_ret = min_ret, ptf_var = pf_var )

  for( i in 2: size_out ) {
    eff_weights <- ptf_wts( data_df, date_var, bullet_ret[ i ] )
    pf_var <- as.numeric( t( eff_weights ) %*% S %*% eff_weights )

    bullet.df <- bind_rows(
      bullet.df,
      data_frame( E_ret = bullet_ret[ i ], ptf_var = pf_var ) )
  }

  return( bullet.df )

}
