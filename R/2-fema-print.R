print.fema <- function(x){
  cat("\nInput Attributes\n")
  cat("\nGroups:", nrow(x$dmat))
  cat("\nSlopes:", paste(colnames(x$slopes), collapse = ', '))
  cat("\nLongitude-Latitude:", x$longlat)
  cat("\nCorrelation Function:", x$ctype)
}