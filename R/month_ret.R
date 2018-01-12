#' make monthly stock return or variance
#'
#' This function makes monthly stock returns or variances using one of three
#'   different methods specified by type.
#'   Type can take on the values: mean, cum, duffee, var
#' @param data_df data frame
#' @param date_var the name of the date variable in quotes. Default to date_id
#' @param type the method used for calculating the return or variance
#' @keywords returns
#' @import magrittr
#' @import dplyr
#' @import lubridate
#' @export
#' @examples
#' month_ret()

month_ret <- function( data_df, date_var = "date_id", type ) {

  # make compatible and break if not
  type <- tolower( type )
  if( type %!in% c( "mean", "cum", "duffee", "var") ) {
    print( "Type not valid")
  } else{
    # call make_ret to transform prices into daily returns
    data_df %<>% make_ret( date_var )

    tic_list <- data_df %>% select_( paste0("-", date_var) ) %>% names( )

    for( t in 1:length(tic_list) ) {
      tic_start <- data_df %>%
        select_( date_var, tic_list[t] ) %>%
        filter( is.na( tic_list[t] ) == FALSE ) %>%
        min( .$date_var )

      if( t == 1 ) { data_start <- tic_start }
      else { if(tic_start > data_start) {
        data_start <- tic_start
      }
      }

    }

    return( data_start )

    out.df <- data_df %>%
      mutate( year_mon = as.yearmon( date_var ) ) %>%
      group_by( year_mon ) %>%
      summarize( N_days = n() )
    #return( out.df )

  }




}

'%!in%' <- function(x,y)!('%in%'(x,y))

# summarize( N_days = n(),
#            mnthly_ret = mean( ret ) -1,
#            duffee_ret = sum( log( ret ) ),
#            cum_ret    = sum( ret - 1 ),
#            mnthly_var = var( ret ) ) %>%
#   mutate( mnthly_ret = mnthly_ret * N_days *100,
#           cum_ret    = cum_ret *100,
#           duffee_ret = duffee_ret *100,
#           mnthly_var = 100^2 * mnthly_var ) %>%
#   # drop the last month with only a few obs
#   filter(N_days > 15) %>%
#   select( -N_days) %>%
#   # reduce the degrees of precision
#   mutate_if( is.numeric, funs( round(.,  digits = 2) ) )
