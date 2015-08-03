#' get.psth
#'
#' @description calculate a PSTH histogram
#' @param spikes
#' @param binwidth im ms
#' @param samplerate in Hertz
#'
#' @return psth data
#' @export
#'
get.psth <- function(spikes, binwidth = 10, samplerate = 25000) {
	# calculate samples per bin
	samplesperbin <- round(binwidth * samplerate / 1000)
	# calculate the number of bins
	n.bins <- floor(nrow(spikes) / samplesperbin)
	# preallocate a vector of length equal to the number of bins
	count <- vector("numeric", n.bins)
	# loop over bins to count spikes
	for (i in 1:n.bins) {
		bin <- spikes[(1 + (i - 1) * samplesperbin):(i * samplesperbin), ]
		# count the number of spikes
		count[i] <- sum(bin[,])
	}
	# normalise count by the number of trials
	normalcount <- count / ncol(spikes)
	binspersecond <- 1000 / binwidth
	# calculate psth: spikes / bin * bin / s
	psth <- normalcount * binspersecond
	return(psth)
}
