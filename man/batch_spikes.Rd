\name{batch_spikes}
\alias{batch_spikes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
batch_spikes(rawdata, std_factor = 1, min_isi = 1, sigma = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawdata}{
%%     ~~Describe \code{rawdata} here~~
}
  \item{std_factor}{
%%     ~~Describe \code{std_factor} here~~
}
  \item{min_isi}{
%%     ~~Describe \code{min_isi} here~~
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
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
function (rawdata, std_factor = 1, min_isi = 1, sigma = NA) 
{
    filenames <- unlist(data[1])
    rawdata <- rawdata[2]
    spikes <- array(data = NA_real_, dim = c(dim(rawdata[[1]][[1]])[1], 
        length(filenames), 2))
    s_per_ms <- 25
    for (i in 1:length(filenames)) {
        signal <- get_H1signal(rawdata[[1]][[i]])
        if (is.na("sigma") == FALSE) {
        }
        m <- c(mean(signal[, 1]), mean(signal[, 2]))
        s <- c(sd(signal[, 1]), sd(signal[, 2]))
        thres1 <- m[1] + s[1] * std_factor
        thres2 <- m[2] + s[2] * std_factor
        rsp1 <- threshold_spikes(signal[, 1], thres1, file = filenames[i])
        rsp2 <- threshold_spikes(signal[, 2], thres2, file = filenames[i])
        rsp <- cbind(rsp1, rsp2)
        spikes[, i, ] <- rsp
    }
    print("fin")
    return(spikes)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
