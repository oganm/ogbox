\name{sourceGithub}
\alias{sourceGithub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sourceGithub(user, repo, script)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{user}{
%%     ~~Describe \code{user} here~~
}
  \item{repo}{
%%     ~~Describe \code{repo} here~~
}
  \item{script}{
%%     ~~Describe \code{script} here~~
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
function (user, repo, script) 
{
    user = substitute(user)
    user = as.character(user)
    repo = substitute(repo)
    repo = as.character(repo)
    script = substitute(script)
    script = as.character(script)
    require(RCurl)
    if (!grepl("[.](r|R)", script)) {
        script = paste0(script, ".R")
    }
    text = getURL(paste0("https://raw.githubusercontent.com/", 
        user, "/", repo, "/master/", script), ssl.verifypeer = FALSE)
    source(textConnection(text))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
