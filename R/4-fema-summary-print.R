print.summary.fema <- function(x){
  cat("\nField Measures\n")
  cat("\nTotal Effect:", x$total.effect)
  cat("\nRange:", x$range)
  cat("\nStandardized Effect:", x$std.effect)
  cat("\nStrength:", x$strength)
}