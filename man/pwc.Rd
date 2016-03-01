\name{pwc}
\alias{pwc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pwc(x1, x2, ctype)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
%%     ~~Describe \code{x1} here~~
}
  \item{x2}{
%%     ~~Describe \code{x2} here~~
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
function (x1, x2, ctype) 
{
    x1x2 <- data.frame(x1 = x1, x2 = x2)
    if (ctype == "single1") {
        x1x2 <- rbind(1, x1x2)
        cpx1 <- with(x1x2, crossprod(x1))
        cpx2 <- with(x1x2, crossprod(x2))
        cpx1x2 <- with(x1x2, crossprod(x1, x2))
        r <- cpx1x2/sqrt(cpx1 * cpx2)
    }
    if (ctype == "single2") 
        r <- with(x1x2, (pi - 2 * abs(x1 - x2))/pi)
    if (ctype == "multi") 
        r <- with(x1x2, sqrt(sum((x1 - x2)^2)))
    r
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
