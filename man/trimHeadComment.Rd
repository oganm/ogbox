\name{trimHeadComment}
\alias{trimHeadComment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
trimHeadComment(fileName, commentChar = "#", outFile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fileName}{
%%     ~~Describe \code{fileName} here~~
}
  \item{commentChar}{
%%     ~~Describe \code{commentChar} here~~
}
  \item{outFile}{
%%     ~~Describe \code{outFile} here~~
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
function (fileName, commentChar = "#", outFile = NULL) 
{
    lines = readLines(fileName)
    i = 1
    while (T) {
        if (substr(lines[i], 1, 1) != commentChar) {
            break
        }
        i = i + 1
    }
    if (is.null(outFile)) {
        return(textConnection(paste(lines[i:length(lines)], collapse = "\n")))
    }
    else {
        fileConn = file(outFile)
        writeLines(text = lines[i:length(lines)], con = fileConn)
        close(fileConn)
        invisible(textConnection(paste(lines[i:length(lines)], 
            collapse = "\n")))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
