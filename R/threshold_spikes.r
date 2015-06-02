threshold_spikes <- function(signal, threshold, file) {
	#' \code{threshold_spikes} apply a threshold to the input signal
	#' @param signal numeric
	#' @param threshold
	#' @param file
	#' 
	
	# check where the signal exceeds the threshold an convert the resulting vector to numeric values of 0 and 1
	ts <- (signal > threshold) + 0
	
	starts <- which(diff(ts) > 0) + 1
	ends <- which(diff(ts) < 0) + 1
	
	# starts and ends may no have any content. let’s check
	if(is.na(starts[1] > ends[1])) {
		print(c("Error, start and end are not comparable.", file))
		spikes <- vector("numeric", length(signal))
		return(spikes)
	}
	# signal may start or end above threshold
	if (starts[1] > ends[1]) {
		starts <- c(1, starts)
	}
	if (length(starts) > length(ends)) {
		ends <- c(ends, length(signal))
	}
	# preallocate spike vector
	spikes <- vector("numeric", length(signal))
	for (i in 1:length(starts)) {
		segment <- signal[starts[i]:ends[i]]
	}
	# get the position of max value
	pos <- which.max(segment)
	spikes[starts[i] + pos - 1] <- 1

	return(spikes)
}
