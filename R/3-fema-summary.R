summary.fema <- function(x) {
  total.effect <- sum(x$rmat * x$dmat)
  range <- total.effect / sum(x$rmat)
  std.effect <- total.effect / sum(x$dmat)
  bdis <- apply(x$slopes, 1, function(z) sqrt(sum(z^2)))
  strength <- std.effect * mean(bdis)
  result <- list(coords = x$coords, slopes = x$slopes, 
                 rmat = x$rmat, dmat = x$dmat, 
                 total.effect = total.effect, 
                 range = range, 
                 std.effect = std.effect, 
                 strength = strength)  
  class(result) <- c('summary.fema', 'fema')
  result
}