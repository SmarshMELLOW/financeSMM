#' Create stock returns for different durations.
#'
#' This function takes a data frame of stock prices and a date variable created
#'   by get_close and creates a new data frame with returns for different
#'   durations. The default options are daily, weekly, monthly, and total. By
#'   specifying a number for per.len, the returns will be for a period of that
#'   many days.
#' @param df data frame
#' @param per.len character. The length of the period. E.g. weekly, monthly, daily, total
#' @param per.st the start of the period. Default to 1. For weekdays, 1 corresponds to Sunday.
#' @param ann If TRUE, the returns be annualized. Default to FALSE
#' @param quietly FALSE, If TRUE, then it will not print the period length
#' @keywords return variance
#' @import magrittr
#' @import dplyr
#' @export
#' @examples
#' ret_type()

ret_type <- function( df, per.len, per.st = 1, ann = FALSE, queitly = FALSE ) {

  if (is.numeric(per.len) == FALSE) {per.len <- tolower(per.len)}

  ret.df <- df %>% fill_nm() %>% make_ret()  # make the df into daily returns

  # daily returns ------------------------------------------
  if (per.len == "daily") {
    out.df <- ret.df
  }

  # weekly returns ------------------------------------------
  if (per.len == "weekly") {
    temp.df <- ret.df %>%
      mutate( week.per = floor((date_id - min(date_id))/7) + 1 ) %>%
      group_by( week.per)

    out.df <- merge(
      temp.df %>% summarise(week.st = first(date_id), N_days = n()),
      temp.df %>% summarise_if( is.numeric, funs( (sum(. - 1) * 100) ) ),
      by = "week.per")

    # remove starting and ending weeks if they are only partial weeks
    if (out.df$N_days[1] < 5) {out.df <- out.df[2:nrow(out.df),]}
    if (out.df$N_days[nrow(out.df)] < 5) {out.df <- out.df[1:(nrow(out.df)-1),]}

    # remove temp vars
    out.df %<>% ungroup() %>% select(-week.per, -N_days)
  }

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

    # temp.df <- ret.df %>%
    #   mutate(ym_date = as.yearmon( date_id) ) %>%
    #   group_by( ym_date )
    #
    # out.df <- merge(
    #   temp.df %>% summarise( N_days = n()),
    #   temp.df %>% summarise_if( is.numeric, funs( (sum(. - 1) * 100) ) ),
    #   by = "ym_date" ) %>%
    #   filter(N_days > 15) %>%
    #   ungroup() %>%
    #   select( -N_days)


  }

  # total returns -----------------------------------------------------------
  if (per.len == "total") {
    dt.start <- min(ret.df$date_id) - days(1)
    dt.end <- max(ret.df$date_id)

    out.df <- ret.df %>%
      mutate_if(is.numeric, funs(cumprod)) %>%
      filter(date_id == dt.end) %>%
      select(-date_id)
  }

  # other frequency returns ------------------------------------------
  if (is.numeric(per.len) == TRUE) {
    temp.df <- ret.df %>%
      mutate( period.id = floor((date_id - min(date_id))/per.len) + 1 ) %>%
      group_by( period.id)

    out.df <- merge(
      temp.df %>% summarise(period.st = first(date_id), N_days = n()),
      temp.df %>% summarise_if( is.numeric, funs( (sum(. - 1) * 100) ) ),
      by = "period.id")

    # remove temp vars and display note about return
    print(paste0("Returns are for ", per.len, " day periods"))
    out.df %<>% ungroup() %>% select(-period.id, -N_days)
  }

  # print the type of returns if specified ------------------------------------
  if (quietly == FALSE) {
    if(per.len %in% c("daily", "weekly", "monthly")) {
      print(paste0("Retunrs are ", per.len))
    }

    if(per.len == "total") {
        print(pasate0("Total Returns for period ", dt.start, " - ", dt.end))
    }

    if (is.numeric(per.len) == TRUE) {
      print(paste0("Returns are for ", per.len, " day periods"))
    }
  }


  return( out.df )

}
