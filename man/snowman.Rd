\name{snowman}
\alias{snowman}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
snowman(x)
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
    W = c("_===_", " ___\n .....", "  _\n  /_\\", " ___\n (_*_)", 
        ",", ".", "_", " ", ".", "o", "O", "-", " ", "\\", " ", 
        " ", "<", " ", "/", " ", " ", "/", " ", "", ">", " ", 
        "\\", "", " : ", "] [", "> <", "   ", " : ", "\" \"", 
        "___", "   ")
    i = as.integer(strsplit(x, "")[[1]])
    cat(" ", W[i[1]], "\n", W[i[5] + 12], "(", W[i[3] + 8], W[i[2] + 
        4], W[i[4] + 8], ")", W[i[6] + 20], "\n", W[i[5] + 16], 
        "(", W[i[7] + 28], ")", W[i[6] + 24], "\n", " (", W[i[8] + 
            32], ")", sep = "")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
