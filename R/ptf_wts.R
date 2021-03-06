#' Calculate the portfolio weights of each stock for the Markowitz efficient ptf
#'
#' In more detail ... This function
#' @param df the dataframe to be manipulated
#' @param ptf_mu the E ret of the portfolio
#' @param tic.names FALSE. If TRUE, return a df with tickers and weights
#' @keywords finance
#' @import magrittr
#' @import dplyr
#' @import lubridate
#' @export
#' @examples
#' ptf_weights()
#'
ptf_wts <- function( df, ptf_mu = NA, tic.names = FALSE) {

  # make the vcov matrix
  S <- df %>% select_if( is.numeric ) %>% var( )

  # make the vactor of E returns
  mu <- df %>% summarise_if( is.numeric, funs( mean ) ) %>%
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

  # return stocks names if option = TRUE
  if (tic.names == TRUE) {
    out.df <- bind_cols(data_frame(tic = row.names(mu)), as.data.frame(z))
    names(out.df) <- c("tic","wts")
    return( out.df )
  } else {
    return(z )
  }

}

# when use (or allow for) numeric date var
# S <- data_df %>%
#  select_( paste0("-",date_var) ) %>%
#  var( )

