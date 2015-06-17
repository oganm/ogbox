\name{toColor}
\alias{toColor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
toColor(daList, palette = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{daList}{
%%     ~~Describe \code{daList} here~~
}
  \item{palette}{
%%     ~~Describe \code{palette} here~~
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
function (daList, palette = NULL) 
{
    daList = as.factor(daList)
    uniq = unique(daList)
    if (is.null(palette[1])) {
        palette = rainbow(length(uniq))
    }
    if (is.null(names(palette))) {
        names(palette) = uniq
    }
    cols = vector(length = length(daList))
    if (!is.null(names(palette))) {
        palette = trimNAs(palette[match(uniq, names(palette))])
        names(palette) = uniq
    }
    for (i in 1:length(uniq)) {
        cols[daList == uniq[i]] = palette[i]
    }
    out = list()
    out$cols = cols
    out$palette = palette
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
