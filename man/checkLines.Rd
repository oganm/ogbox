\name{checkLines}
\alias{checkLines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
checkLines(daFile, lines, fun = readLines, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{daFile}{
%%     ~~Describe \code{daFile} here~~
}
  \item{lines}{
%%     ~~Describe \code{lines} here~~
}
  \item{fun}{
%%     ~~Describe \code{fun} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (daFile, lines, fun = readLines, ...) 
{
    outAll = vector(mode = "list", length = length(lines))
    for (i in 1:length(lines)) {
        con = pipe(paste0("sed -n -e'", lines[i], "p' ", daFile))
        out = fun(con, ...)
        outAll[[i]] = out
    }
    return(outAll)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
