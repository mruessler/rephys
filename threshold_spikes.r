threshold_spikes <- function(signal, threshold, file) {
	# apply a threshold
	ts <- ifelse(signal > threshold, 1, 0)
	
# 	look for start/end of the sections:
# 	diff of a vector of logical values gives:
# 	 1 - if v(n-1)=0 and v(n)=1
# 	 0 - if v(n-1)==v(n)
# 	-1 - if v(n-1)=1 and v(n)=0
	
	starts <- which(diff(ts) > 0) + 1
	ends <- which(diff(ts) < 0) + 1
	
	# signal may start or end above threshold
	if(is.na(starts[1] > ends[1])) {
		print(c("error, start and end are not comparable.", file))
		
	}
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
