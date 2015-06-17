\name{sepExpr}
\alias{sepExpr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sepExpr(allDataPre)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{allDataPre}{
%%     ~~Describe \code{allDataPre} here~~
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
function (allDataPre) 
{
    if (class(allDataPre)[1] == "data.frame") {
        for (i in 1:ncol(allDataPre)) {
            if ("double" == typeof(allDataPre[, i])) {
                expBound = i
                break
            }
        }
        geneData = allDataPre[, 1:(expBound - 1)]
        exprData = allDataPre[, expBound:ncol(allDataPre)]
        return(list(geneData, exprData))
    }
    else if (class(allDataPre)[1] == "data.table") {
        for (i in 1:ncol(allDataPre)) {
            if ("double" == typeof(allDataPre[[i]])) {
                expBound = i
                break
            }
        }
        geneData = allDataPre[, 1:(expBound - 1), with = F]
        exprData = allDataPre[, expBound:ncol(allDataPre), with = F]
        return(list(geneData, exprData))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
