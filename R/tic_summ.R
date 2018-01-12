#' Return the avg ret and sd for individual stocks
#'
#' This function takes a dataframe with stock returns and calculates the mean
#'   return and the standard deviation of the return for each stock.
#' @param data_df the dataframe to be manipulated
#' @param date_var the name of the date variable in quotes. Default to date_id
#' @keywords finance mean sd
#' @import magrittr
#' @import dplyr
#' @export
#' @examples
#' tic_summ()
#'
# first make a matrix with the stand alone avg return and variance
tic_summ <- function( data_df, date_var = "date_id" ) {

  data_df %<>% select_( paste0("-",date_var) )

  s_m <- summarise_each(data_df, funs(mean) ) %>% gather(key = tic, value = mean )
  s_sd <- summarise_each(data_df, funs(sd) ) %>% gather(key = tic, value = sd  )

  out.df <- merge(s_m, s_sd)

}

