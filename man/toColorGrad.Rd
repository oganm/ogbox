\name{toColorGrad}
\alias{toColorGrad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
toColorGrad(daList, startCol = "white", endCol = "red", fine = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{daList}{
%%     ~~Describe \code{daList} here~~
}
  \item{startCol}{
%%     ~~Describe \code{startCol} here~~
}
  \item{endCol}{
%%     ~~Describe \code{endCol} here~~
}
  \item{fine}{
%%     ~~Describe \code{fine} here~~
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
function (daList, startCol = "white", endCol = "red", fine = 0.01) 
{
    colorGrad = colorRampPalette(c(startCol, endCol))
    colorLegend = data.frame(value = seq(min(daList), max(daList), 
        fine), color = colorGrad(len(seq(min(daList), max(daList), 
        fine))))
    colors = colorLegend$color[sapply(daList, function(x) {
        which(abs(colorLegend$value - x) == min(abs(colorLegend$value - 
            x)))
    })]
    out = list(cols = colors, palette = colorLegend)
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
