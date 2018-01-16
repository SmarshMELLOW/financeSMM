# utilities file for package
make_ret <- function( df ) {

  # make df into returns
  df %<>% mutate_if(is.numeric, funs(. / lag(.) ) )

  df <- df[2:nrow(df),]
  return(df)

}

'%!in%' <- function(x,y)!('%in%'(x,y))

