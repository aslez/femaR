\name{gwr2fema}
\alias{gwr2fema}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gwr2fema(x, slopes = NULL, intercept = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{slopes}{
%%     ~~Describe \code{slopes} here~~
}
  \item{intercept}{
%%     ~~Describe \code{intercept} here~~
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
function (x, slopes = NULL, intercept = FALSE) 
{
    if (class(x) != "gwr") {
        stop("x is not a gwr-class object")
    }
    cnames <- names(coef(x$lm))
    iloc <- grep("(Intercept)", cnames)
    if (!intercept) 
        cnames <- cnames[-iloc]
    if (!is.null(slopes)) {
        sloc <- sapply(slopes, function(z) grep(z, cnames))
        if (intercept) 
            cnames <- cnames[c(iloc, sloc)]
        else cnames <- cnames[sloc]
    }
    df <- as.data.frame(x$SDF[, cnames])
    class(df) <- c("fema.df", "data.frame")
    df
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
