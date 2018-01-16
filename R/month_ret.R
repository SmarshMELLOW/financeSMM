#' make monthly stock return or variance
#'
#' This function makes monthly stock returns or variances using one of three
#'   different methods specified by type.
#'   Type can take on the values: mean, cum, duffee, var
#' @param data_df data frame
#' @param date_var the name of the date variable in quotes. Default to date_id
#' @param type string. The method used for calculating the return or variance
#' @param round integer. Option to reduce the degrees of precision. Default to false.
#' @keywords returns
#' @import magrittr
#' @import dplyr
#' @import lubridate
#' @export
#' @examples
#' month_ret()

month_ret <- function( data_df, date_var = "date_id", type, round = FALSE ) {

  # make compatible and break if not
  type <- tolower( type )
  if( type %!in% c( "mean", "cum", "duffee", "var") ) {
    print( "Type not valid")
  } else{
    # call make_ret to transform prices into daily returns
    data_df %<>% make_ret( ) %>%
      # if using another name will have to fix later
      mutate(ym_date = as.yearmon( date_id) ) %>%
      group_by( ym_date )

    # calculate return type ---------------------------------------------------
    # 1. mean daily returns
    if (type == "mean") {
      out_df <- merge( data_df %>% summarise( N_days = n()),
                       data_df %>% summarise_if( is.numeric, funs( mean(.) - 1) ),
                       by = "ym_date" ) %>%
        mutate_at( vars(3:ncol(.)), funs(. * N_days *100 ) ) %>%
        select( -N_days)
    }

    # 2. cumulative returns
    if (type == "cum") {
      out_df <- data_df %>%
        summarise_if( is.numeric, funs( (sum(. - 1) * 100) ) )
    }
    # 3. Duffee type returns
    if (type == "duffee") {
      # cum_ret    = sum( log( ret ) )
      out_df <- data_df %>%
        summarise_if( is.numeric, funs( sum( log(.) ) * 100) )
    }
    # 4. variance
    if (type == "var") {
      out_df <- data_df %>%
        summarise_if( is.numeric, funs( var(.) * 100^2) )
    }
    # -------------------------------------------------------------------
    # filter out months with less than 15 days
    out_df %<>%
      merge( data_df %>% summarise( N_days = n()), by = "ym_date" ) %>%
      filter(N_days > 15) %>%
      select( -N_days)

    # reduce the precision if option specified
    if ( round != FALSE ) {
      out_df %<>% mutate_if( is.numeric, funs( round(.,  digits = round) ) )
    }

    return( out_df )

  }
}

# summarize( N_days = n(),
#            mnthly_ret = mean( ret ) -1,
#            duffee_ret = sum( log( ret ) ),
#            cum_ret    = sum( ret - 1 ),
#            mnthly_var = var( ret ) ) %>%
#   mutate( mnthly_ret = mnthly_ret * N_days *100,
#           cum_ret    = cum_ret *100,
#           duffee_ret = duffee_ret *100,
#           mnthly_var = 100^2 * mnthly_var ) %>%
