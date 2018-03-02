#' Create stock returns for different durations.
#'
#' This function takes a data frame of stock prices and a date variable created
#'   by get_close and creates a new data frame with returns for different
#'   durations. The default options are daily, weekly, monthly, and total. By
#'   specifying a number for per.len, the returns will be for a period of that
#'   many days.
#' @param df data frame
#' @param per.len character or number. The length of the period. E.g. weekly, monthly, daily, total
#' @param per.st the start of the period. Default to 1. For weekdays, 1 corresponds to Sunday.
#' @param ann If TRUE, the returns be annualized. Default to FALSE
#' @param quietly TRUE, If FALSE, then it will print the period length
#' @keywords return variance
#' @import magrittr
#' @import dplyr
#' @export
#' @examples
#' ret_type()

ret_type <- function( df, per.len, per.st = 1, ann = FALSE, quietly = TRUE ) {

  # Still to add annualized return method and method to change the period
  #  start date (as of 3-2-18)

  if (is.numeric(per.len) == FALSE) {per.len <- tolower(per.len)}

  # If quietly = FALSE, print the type of returns -----------------------------
  if (quietly == FALSE) {
    if(per.len %in% c("daily", "weekly", "monthly")) {
      print(paste0("Retunrs are ", per.len))
    }

    if(per.len == "total") {
      dt.start <- min(df$date_id)
      dt.end <- max(df$date_id)
      print(paste0("Total Returns for period ", dt.start, " - ", dt.end))
    }

    if (is.numeric(per.len) == TRUE) {
      print(paste0("Returns are for ", per.len, " day periods"))
    }
  }
  # ---------------------------------------------------------------------------

  ret.df <- df %>% fill_nm() %>% make_ret()  # make the df into daily returns

  # daily returns ------------------------------------------
  if (per.len == "daily") {
    out.df <- ret.df
  }

  # total returns -----------------------------------------------------------
  if (per.len == "total") {

    out.df <- ret.df %>%
      mutate_if(is.numeric, funs(cumprod)) %>%
      mutate(period.date = max(date_id)) %>%
      filter(date_id == period.date) %>%
      select(-date_id) %>%
      select(period.date, everything()) %>%
      mutate_at(vars(-period.date), funs((. - 1) * 100))
  }

  # weekly returns ------------------------------------------
  if (per.len == "weekly") { per.len <- 7}

  # monthly returns -----------------------------------------------------------
  if (per.len == "monthly") {
    out.df <- ret.df %>%
      mutate(ym_date = as.yearmon( date_id) ) %>%
      group_by( ym_date ) %>%
      mutate_if(is.numeric, funs(cumprod)) %>%
      mutate(l.date = max(date_id),
             N_days = n()) %>%
      filter( date_id == l.date)  %>%
      filter(N_days > 15) %>%
      ungroup() %>%
      select( -N_days, -l.date, -date_id) %>%
      select(ym_date, everything()) %>%
      mutate_at(vars(-ym_date), funs((. - 1) * 100))

  }

  # numeric frequency returns -------------------------------------------------
  if (is.numeric(per.len) == TRUE) {
    out.df <- ret.df %>%
      mutate( period.id = floor((date_id - min(date_id))/per.len) + 1 ) %>%
      group_by( period.id) %>%
      mutate_if(is.numeric, funs(cumprod)) %>%
      mutate(period.date = max(date_id),
             N_days = n()) %>%
      filter( date_id == period.date)

    # remove starting and ending periods if they are only partial periods
    if (out.df$N_days[1] < per.len / 2) {out.df <- out.df[2:nrow(out.df),]}
    if (out.df$N_days[nrow(out.df)] < per.len / 2) {out.df <- out.df[1:(nrow(out.df)-1),]}

    out.df %<>% ungroup() %>%
      select( -N_days, -date_id) %>%
      select(period.date, period.id, everything()) %>%
      mutate_at(vars(-period.date, -period.id), funs((. - 1) * 100))
  }

  return( out.df )

}


