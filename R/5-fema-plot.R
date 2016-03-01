plot.fema <- function(x) {
  dmat <- x$dmat
  diag(dmat) <- NA
  rmat <- x$rmat
  diag(rmat) <- NA
  df <- data.frame(distance = as.vector(dmat),
                   correlation = as.vector(rmat))
  ggplot(df, aes(x = distance, y = correlation)) +
    geom_point(alpha = .3) +
    stat_smooth(method = 'loess', colour = 'red') +
    theme_bw()
}