\name{discorr}
\alias{discorr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
discorr(df, longlat = FALSE, ctype = "single1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{longlat}{
%%     ~~Describe \code{longlat} here~~
}
  \item{ctype}{
%%     ~~Describe \code{ctype} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (df, longlat = FALSE, ctype = "single1") 
{
    if (!"fema.df" \%in\% class(df)) {
        stop("df is not a fema-class data frame")
    }
    coords <- cbind(df$coord.x, df$coord.y)
    dmat <- spDists(coords, longlat = longlat)
    cloc <- sapply(c("coord.x", "coord.y"), function(x) grep(x, 
        names(df)))
    slopes <- data.frame(df[, -cloc])
    if (NCOL(slopes) > 1) 
        ctype <- "multi"
    names(slopes) <- names(df)[-cloc]
    rmat <- matrix(0, nrow(dmat), ncol(dmat))
    for (i in 2:nrow(slopes)) {
        for (j in 1:(i - 1)) {
            rmat[i, j] = pwc(slopes[i, ], slopes[j, ], ctype)
        }
    }
    if (ctype == "multi") {
        rmat <- (max(rmat) - 2 * rmat)/max(rmat)
    }
    rmat <- rmat + t(rmat)
    result <- list(coords = coords, slopes = slopes, longlat = longlat, 
        ctype = ctype, dmat = dmat, rmat = rmat)
    class(result) <- "fema"
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
