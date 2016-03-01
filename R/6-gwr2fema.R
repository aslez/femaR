#'Convert GWR Output to FEMA Data Frame
#'
#'\code{gwr2fema} converts output produced using \code{gwr} to a data frame
#'that can be read by \code{discorr}.
#'
#'@param x a gwr-class object
#'
#'@param slopes a string vector indicating which slopes to retain.  The default
#'is \code{NULL}, which retains all slopes.
#'
#'@param intercept logical indicating whether or not to return the intercept.
#'The default is \code{FALSE}.
#'
#'@return The function \code{gwr2fema} returns a fema-class data frame.


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