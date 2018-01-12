#' Get detailed ETF data from ishares
#'
#' This function scrapes an ishares url and selects the relevant variables
#'   and formats it to be used in R. Option to save the dataframe
#' @param ishares_url the url of the file to be downloaded
#' @param etf the name of the etf that is downloaded
#' @param save_dir directory to save the file, default to FALSE
#' @keywords finance ishares scrape import
#' @import magrittr
#' @import dplyr
#' @export
#' @examples
#' read_ishares()
#'

read_ishares <- function( ishares_url, etf, save_dir = FALSE ) {
  # the first 10 lines are junk
  etf.desc <- read.csv( ishares_url, skip = 10, check.names = TRUE)

  # names are not workable--fix
  names(etf.desc ) <- tolower( names(etf.desc ) )

  etf.desc %<>% dplyr::rename( tic = ticker,
                               com_name = name,
                               ptf_wgt = weight....,
                               market_value = market.value,
                               asset_class = asset.class) %>%
    select( tic, com_name, ptf_wgt, price, shares, market_value, asset_class,
            sector, exchange) %>%
    # can't use factors--fix
    mutate_all( funs( as.character(.) ) ) %>%
    filter( is.na(ptf_wgt) == FALSE ) %>%
    mutate_at( vars(shares, market_value ),
               funs( as.numeric( gsub(",", "", . )) ) ) %>%
    mutate_at( vars( ptf_wgt, price),
               funs( as.numeric(. ) ) ) %>%
    # for use with scraping data
    mutate( exchange = ifelse( exchange == "New York Stock Exchange Inc.",
                               "NYSE", exchange),
            # if it is a class a stock, then the dot is missing--fix
            class.a = grepl("CLASS", com_name),
            tic.len = nchar( tic ),
            tic     = ifelse(
              class.a == TRUE & substring(tic, tic.len) == "A",
              paste0( substring(tic, 1, tic.len -1), ".", "A"),
              tic ) ) %>%
    select( -class.a, -tic.len)

  test.df <- ITA.desc %>%
    mutate( class.a = grepl("CLASS", com_name),
            tic.len = nchar( tic ),
            tic.adj = ifelse( class.a == TRUE &
                                substring(tic, tic.len) == "A",
                              paste0(
                                substring(tic, 1, tic.len -1), ".", "A"), tic ) )

  # write to disk and include date for ref if asked for
  if( save_dir != FALSE ) {
    saveRDS( etf.desc,
             file = paste0( save_dir, etf, "desc", Sys.Date(), ".rds") )
  }

  return( etf.desc)
}

