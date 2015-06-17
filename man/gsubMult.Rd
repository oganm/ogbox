\name{gsubMult}
\alias{gsubMult}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gsubMult(patterns, replacements, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{patterns}{
%%     ~~Describe \code{patterns} here~~
}
  \item{replacements}{
%%     ~~Describe \code{replacements} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{ignore.case}{
%%     ~~Describe \code{ignore.case} here~~
}
  \item{perl}{
%%     ~~Describe \code{perl} here~~
}
  \item{fixed}{
%%     ~~Describe \code{fixed} here~~
}
  \item{useBytes}{
%%     ~~Describe \code{useBytes} here~~
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
function (patterns, replacements, x, ignore.case = FALSE, perl = FALSE, 
    fixed = FALSE, useBytes = FALSE) 
{
    for (i in 1:length(patterns)) {
        x = gsub(patterns[i], replacements[i], x, ignore.case, 
            perl, fixed, useBytes)
    }
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
