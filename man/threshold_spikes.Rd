\name{threshold_spikes}
\alias{threshold_spikes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
threshold_spikes(signal, threshold, file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{signal}{
%%     ~~Describe \code{signal} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
}
  \item{file}{
%%     ~~Describe \code{file} here~~
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
function (signal, threshold, file) 
{
    ts <- ifelse(signal > threshold, 1, 0)
    starts <- which(diff(ts) > 0) + 1
    ends <- which(diff(ts) < 0) + 1
    if (is.na(starts[1] > ends[1])) {
        print(c("error, start and end are not comparable.", file))
    }
    if (starts[1] > ends[1]) {
        starts <- c(1, starts)
    }
    if (length(starts) > length(ends)) {
        ends <- c(ends, length(signal))
    }
    spikes <- vector("numeric", length(signal))
    for (i in 1:length(starts)) {
        segment <- signal[starts[i]:ends[i]]
    }
    pos <- which.max(segment)
    spikes[starts[i] + pos - 1] <- 1
    return(spikes)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
