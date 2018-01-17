# utilities file for package

# make df into returns -------------------------------
make_ret <- function( df ) {

  df %<>% mutate_if(is.numeric, funs(. / lag(.) ) )

  df <- df[2:nrow(df),]
  return(df)
}

# not in function ----------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

# fill in random days that are unpopulated with previous close -----------------
fill_nm <- function( df ) {
  df %<>% mutate_if(
    is.numeric, funs( ifelse( is.na(lag(.)) == FALSE & is.na(.) == TRUE,
                                               lag(.), .)))
}

