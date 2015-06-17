\name{legendGrad}
\alias{legendGrad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
legendGrad(col, lev)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{lev}{
%%     ~~Describe \code{lev} here~~
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
function (col, lev) 
{
    opar <- par
    n <- length(col)
    bx <- par("usr")
    box.cx <- c(bx[2] + (bx[2] - bx[1])/1000, bx[2] + (bx[2] - 
        bx[1])/1000 + (bx[2] - bx[1])/50)
    box.cy <- c(bx[3], bx[3])
    box.sy <- (bx[4] - bx[3])/n
    xx <- rep(box.cx, each = 2)
    par(xpd = TRUE)
    for (i in 1:n) {
        yy <- c(box.cy[1] + (box.sy * (i - 1)), box.cy[1] + (box.sy * 
            (i)), box.cy[1] + (box.sy * (i)), box.cy[1] + (box.sy * 
            (i - 1)))
        polygon(xx, yy, col = col[i], border = col[i])
    }
    par(new = TRUE)
    plot(0, 0, type = "n", ylim = c(min(lev), max(lev)), yaxt = "n", 
        ylab = "", xaxt = "n", xlab = "", frame.plot = FALSE)
    axis(side = 4, las = 2, tick = FALSE, line = 0.25)
    par <- opar
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
