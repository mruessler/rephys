threshold_spikes <- function(signal, threshold, file) {
	#' \code{threshold_spikes} apply a threshold to the input signal and return a vector of 0 and 1
	#' @param signal numeric vector of ephys data
	#' @param threshold
	#' @param file
	#'
	print(threshold)
	# threshold <- - 0.1
	# print(threshold)
	# check where the signal exceeds the threshold an convert the resulting vector from logical to numeric values of 0 and 1
	ts <- (signal > threshold) + 0
	# look for start/end of continuous sections:
	# diff of a vector of logical values gives:
	#    1 if v(n - 1) = 0 and v(n) = 1
	#    0 if v(n - 1) == v(n)
	#   -1 if v(n - 1) = 1 and v(n) = 0
	starts <- which(diff(ts) > 0)# + 1
	ends <- which(diff(ts) < 0)# + 1

	# either starts or ends may no have any content. let’s check and stop in that case
	if (is.na(starts[1] > ends[1])) {
		print(c("Error, start and end are not comparable. Returning empty vector.", file))
		spikes <- vector("numeric", length(signal))
		return(spikes)
	}
	# signal may start or end above threshold
	# if it starts above threshold add 1 to starts
	if (starts[1] > ends[1]) {
		starts <- c(1, starts)
	}
	# if it ends above threshold add the last element to ends
	if (length(starts) > length(ends)) {
		ends <- c(ends, length(signal))
	}
	# preallocate spike vector
	spikes <- vector("numeric", length(signal))
	# create segments of equal value
	for (i in 1:length(starts)) {
		segment <- signal[starts[i]:ends[i]]
		# get the position of max value
		pos <- which.max(segment)
		spikes[starts[i] + pos - 1] <- 1
	}
	return(spikes)
}
