gwr2fema <- function(x, slopes = NULL, intercept = FALSE) {
  if (class(x) != 'gwr') {
    stop('x is not a gwr-class object')
  }
  cnames <- names(coef(x$lm))
  iloc <- grep('(Intercept)', cnames)
  if (!intercept) cnames <- cnames[-iloc]
  if (!is.null(slopes)) {
    sloc <- sapply(slopes, function(z) grep(z, cnames))
    if (intercept) cnames <- cnames[c(iloc, sloc)]
    else cnames <- cnames[sloc]
  }
  df <- as.data.frame(x$SDF[, cnames])
  class(df) <- c('fema.df', 'data.frame')
  df
}