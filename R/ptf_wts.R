#' Calculate the portfolio weights of each stock for the Markowitz efficient ptf
#'
#' In more detail ... This function
#' @param data_df the dataframe to be manipulated
#' @param date_var the name of the date variable in quotes
#' @param ptf_mu the E ret of the portfolio
#' @keywords finance
#' @import magrittr
#' @import dplyr
#' @import lubridate
#' @export
#' @examples
#' ptf_weights()
#'
ptf_wts <- function( data_df, date_var, ptf_mu = NA) {

  # make the vcov matrix
  S <- data_df %>%
    select_( paste0("-",date_var) ) %>%
    var( )

  # make the vactor of E returns
  mu <- data_df %>%
    select_( paste0("-",date_var) ) %>%
    summarise_all( funs( mean ) ) %>%
    as.matrix( ) %>%
    t( )

  # get the number of stocks
  dim_ptf <- nrow( mu )

  #create the A matrix
  A <- rbind(
    cbind( 2 * S, mu,  rep(1, dim_ptf) ),
    cbind( t( mu ), 0, 0),
    cbind( t( rep(1, dim_ptf ) ), 0, 0) )

  # get the inverse of A
  A_inv <- solve( A )

  # if the ptf return is not specified, just do the mean for now
  if( is.na( ptf_mu ) == TRUE ) {
    ptf_mu <- mean( mu )
  }

  # make the b_0 vector
  b <- matrix( data = c( rep(0, dim_ptf ), ptf_mu, 1),
               nrow = dim_ptf + 2, ncol = 1)

  #solution vec is then
  z <- A_inv %*% b

  #subset to only give the ptf weights
  z <- as.matrix( z[1: dim_ptf] )

}

