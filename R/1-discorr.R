#'Construct Distance and Correlation Matrices
#'
#'\code{discorr} is used to construct the distance and correlation matrices 
#'needed to estimate range, organization, and strength.
#'
#'@param df a fema-class data.frame.
#'
#'@param longlat logical indicating whether coordinates are projected.  Default
#'is \code{FALSE}. 
#'
#'@param ctype a string indicating the type of correlation to be caluclated.
#'Valid options include single1 (cosine similarity for a single slope), 
#'single2 (linear similarity for a single slope), 
#'and multi (square root of the sum of squared differences across 
#'multiple slopes). 
#'
#'@param x1 a vector of slopes associated with observation i.
#'
#'@param x2 a vector of slopes associated with observation j.
#' 
#'@details \code{discorr} is used to generate distance and correlation matrices
#'that can then be summarized in terms of range, organization, and strength.  
#'\code{pwc} calculates pairwise correlations using the methods outlined by 
#'Martin, Slez, and Borkenhagen (2016).
#'
#'@return The function \code{discorr} returns the following list of objects:
#' \describe{
#'   \item{coords}{matrix of coordinates.}
#'   \item{slopes}{vectors of slopes.}
#'   \item{longlat}{logical indicating whether coordinates are projected.}
#'   \item{dmat}{the distance matrix.}
#'   \item{rmat}{the correlation matrix.}
#' }
#'
#'@references Martin, J.L., Slez, A., and Borkenhagen, C.  2016.  
#'"Some Provisional Techniques for Quantifying the Degree of Field Effect
#'in Social Data."
#'

discorr <- function(df, longlat = FALSE, ctype = 'single1') {
  if (!'fema.df' %in% class(df)) {
    stop('df is not a fema-class data frame')
  }
  
  #generate distance matrix
  coords <- cbind(df$coord.x, df$coord.y)
  dmat <- spDists(coords, longlat = longlat)
  
  #generate correlations
  cloc <- sapply(c('coord.x', 'coord.y'), function(x) grep(x, names(df)))
  slopes <- data.frame(df[, -cloc])
  if (NCOL(slopes) > 1) ctype <- 'multi'
  names(slopes) <- names(df)[-cloc]
  rmat <- matrix(0, nrow(dmat), ncol(dmat))
  for (i in 2:nrow(slopes)) {
    for (j in 1:(i - 1)) {
      rmat[i, j] = pwc(slopes[i, ], slopes[j, ], ctype)
    }
  }
  if (ctype == 'multi') {
    rmat <- (max(rmat) - 2 * rmat) / max(rmat)
  }
  rmat <- rmat + t(rmat)
  
  #compile results
  result <- list(coords = coords, slopes = slopes, 
                 longlat = longlat, ctype = ctype, 
                 dmat = dmat, rmat = rmat) 
  class(result) <- 'fema'
  result
}

#'@rdname discorr
pwc <- function(x1, x2, ctype) {
  x1x2 <- data.frame(x1 = x1, x2 = x2)
  if (ctype == 'single1') {
    x1x2 <- rbind(1, x1x2)
    cpx1 <- with(x1x2, crossprod(x1))
    cpx2 <- with(x1x2, crossprod(x2))
    cpx1x2 <- with(x1x2, crossprod(x1, x2))
    r <- cpx1x2 / sqrt(cpx1 * cpx2)
  }
  if (ctype == 'single2') r <- with(x1x2, (pi - 2 * abs(x1 - x2)) / pi)
  if (ctype == 'multi') r <- with(x1x2, sqrt(sum((x1 - x2)^2)))
  r
}
