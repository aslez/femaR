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
