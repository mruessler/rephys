get.isi <- function(spikes) {
	# milliseconds per sample
	msps <- 0.04
	lastspike <- 0
	# copy vector
	isi <- spikes
	for (i in 1:length(spikes)) {
		if (spikes[i] == 1) {
			ms <- (i - lastspike) * msps
			# 2 spikes, 1000 ms
			freq <- 2 * 1000 / ms
			if (lastspike > 0) {
				isi[lastspike:i] <- freq
			}
			else {
				isi[1:i] <- freq
			}
			lastspike <- spikes[i]
		}
	}
	return(isi)
}
