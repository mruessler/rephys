#' get.isipsth
#'
#' @description calculate an inter-spike-interval histogram
#' @param spikes matrix of spike data
#' @param binwidth in milliseconds
#' @param samplerate in Hertz
#'
#' @return isipsth matrix
#' @export
#'
get.isipsth <- function(spikes, binwidth = 10, samplerate = 25000) {
	# preallocate a matrix
	freq.estimates <- matrix(data = NA, nrow = nrow(spikes), ncol = ncol(spikes))
	# loop through the columns
	for (i in 1:ncol(spikes)) {
		# get the indices of non-zero elements of the current column
		spikestimes <- which(spikes[, i] != 0)
		# check whether there are actual spikes in the current column
		if (length(spikestimes) > 0) {
			for (j in 2:length(spikestimes)) {
				# get the inter-spike-interval between this and the previous spike
				isi <- spikestimes[j] - spikestimes[j - 1]
				# apply the estimated frequency to all values from the bin
				freq.estimates[(spikestimes[j - 1] + 1):spikestimes[j], i] <- samplerate / isi
			}
			freq.estimates[1:spikestimes[1], i] <- freq.estimates[spikestimes[2], i]
			if (spikestimes[length(spikestimes)] < ncol(freq.estimates)) {
				freq.estimates[(spikestimes[length(spikestimes)] + 1):nrow(freq.estimates), i] <- freq.estimates[spikestimes[length(spikestimes)], i]
			}
		}
	}
	# change NA values to zero
	freq.estimates[!is.finite(freq.estimates)] <- 0
	mean.frequency <- rowMeans(freq.estimates)
	# bin the data
	if (binwidth > 0) {
		samplesperbin <- round(binwidth * samplerate / 1000)
		n.bins <- floor((nrow(spikes)) / samplesperbin)
		# calculate the psth as mean of the bins: transform the frequency vector to n columns with samplesperbin rows and compute the mean of columns
		psth <- colMeans(matrix(mean.frequency[1:(n.bins * samplesperbin)], nrow = samplesperbin, ncol = n.bins))
	}
	else {
		psth <- mean.frequency
	}
	return(psth)
}
