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
