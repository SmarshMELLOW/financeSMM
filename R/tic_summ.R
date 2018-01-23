#' Return the avg ret and variance for individual stocks
#'
#' This function takes a dataframe with stock returns and calculates the mean
#'   return and the standard deviation of the return for each stock.
#' @param df the dataframe to be manipulated
#' @param st.dev FALSE--returns variance, else, sd.
#' @keywords finance mean sd
#' @import magrittr
#' @import dplyr
#' @export
#' @examples
#' tic_summ()
#'
# first make a matrix with the stand alone avg return and variance
tic_summ <- function( df, st.dev = FALSE ) {

  s_m <- df %>% summarise_if(is.numeric, funs(mean) ) %>% gather(key = tic, value = mean )

  if (st.dev == FALSE) {
    s_var <- df %>%
      summarise_if(is.numeric, funs(var) ) %>% gather(key = tic, value = var  )
  } else {
    s_var <- df %>%
      summarise_if(is.numeric, funs(sd) ) %>% gather(key = tic, value = sd  )
  }

  out.df <- merge(s_m, s_var)
}

# old version
# tic_summ <- function( data_df, date_var = "date_id" ) {
#
#   data_df %<>% select_( paste0("-",date_var) )
#
#   s_m <- summarise_all(data_df, funs(mean) ) %>% gather(key = tic, value = mean )
#   s_sd <- summarise_all(data_df, funs(sd) ) %>% gather(key = tic, value = sd  )
#
#   out.df <- merge(s_m, s_sd)
#
# }

