test <- function(spikes) {
	# devtools::load_all();spikes <- autoanalysis()
	# remove all unnecessary lines
	rate <- 25000
	# 2 time windows with borders a/b and c/d
	a <- 0.25 * rate
	b <- 0.75 * rate
	c <- 8.25 * rate
	d <- 8.75 * rate
	# add rownumber
	#spikes <- cbind(1:nrow(spikes), spikes)
	# select the two windows
	winone <- spikes[a:b,]
	wintwo <- spikes[c:d,]
	# calculate some kind of describing number for the two time windows
	windownumbers <- cbind(colSums(winone),colSums(wintwo))
}
