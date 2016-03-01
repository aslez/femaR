\name{summary.fema}
\alias{summary.fema}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.fema(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x) 
{
    total.effect <- sum(x$rmat * x$dmat)
    range <- total.effect/sum(x$rmat)
    std.effect <- total.effect/sum(x$dmat)
    bdis <- apply(x$slopes, 1, function(z) sqrt(sum(z^2)))
    strength <- std.effect * mean(bdis)
    result <- list(coords = x$coords, slopes = x$slopes, rmat = x$rmat, 
        dmat = x$dmat, total.effect = total.effect, range = range, 
        std.effect = std.effect, strength = strength)
    class(result) <- c("summary.fema", "fema")
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
