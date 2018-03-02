#' calculate the efficient frontier
#'
#' This function takes a dataframe of stock prices and calculates the mean-
#'   variance efficient frontier. The period length can be manipulated to
#'   use an alternate time length for calculating the vcov matrix.
#' @param df a dataframe to stock prices
#' @param S_period the period length for the variance matrix. See \code{\link{ret_type}}
#' @param min_ret the minimum return
#' @param max_ret the maximum return
#' @param size_out the number of obs in the returned df
#' @return a dataframe of expected returns and variances
#' @keywords finance markowitz bullet
#' @import magrittr
#' @import dplyr
#' @export
#' @examples
#' solve_frontier()
#'
solve_frontier <- function( df, S_period = "monthly", min_ret = 1.0001,
                         max_ret = 1.5, size_out = 100) {

  # sequence of returns for the bullet
  bullet_ret <- seq( from = min_ret, to = max_ret, length.out = size_out )

  # calculate the A-inv matrix
  #-----------------------------------
  # make the vcov matrix
  S <- ret_type(df, S_period) %>% select_if( is.numeric ) %>% var( )

  # make the vactor of E returns
  mu <- ret_type(df, "monthly") %>% summarise_if( is.numeric, funs( mean ) ) %>%
    as.matrix( ) %>% t( )

  dim_ptf <- nrow( mu )  # get the number of stocks

  #create the A matrix
  A <- rbind(
    cbind( 2 * S, mu,  rep(1, dim_ptf) ),
    cbind( t( mu ), 0, 0),
    cbind( t( rep(1, dim_ptf ) ), 0, 0) )

  # get the inverse of A
  A_inv <- solve( A )
  #-------------------------------------

  bullet.df <- data.frame()

  for( i in 1: size_out ) {
    # make the b_0 vector
    b <- matrix( data = c( rep(0, dim_ptf ), bullet_ret[ i ], 1),
                 nrow = dim_ptf + 2, ncol = 1)

    #solution vec is then
    z <- A_inv %*% b
    eff_weights <- as.matrix( z[1: dim_ptf] )  # subset to only give the ptf weights

    pf_var <- as.numeric( t( eff_weights ) %*% S %*% eff_weights )

    bullet.df <- bind_rows(
      bullet.df,
      data_frame( E_ret = bullet_ret[ i ], ptf_var = pf_var ) )
  }

  return( bullet.df )

}
