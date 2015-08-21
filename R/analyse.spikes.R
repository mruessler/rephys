analyse.spikes <- function(spikes) {
	# devtools::load_all();spikes <- autoanalysis()
	spikes.left <- spikes[, seq(1, ncol(spikes) - 1, 2)]
	spikes.right <- spikes[, seq(2, ncol(spikes), 2)]
	rm(spikes)
	# set samplerate
	rate <- 25000
	# 2 time windows with borders a/b and c/d
	a <- 0.25 * rate
	b <- 0.75 * rate
	c <- 8.25 * rate
	d <- 8.75 * rate
	# add rownumber
	#spikes <- cbind(1:nrow(spikes), spikes)
	# select the two windows
	winone.left <- spikes.left[a:b,]
	winone.right <- spikes.right[a:b,]

	wintwo.left <- spikes.left[c:d,]
	wintwo.right <- spikes.right[c:d,]

	rm(spikes.left)
	rm(spikes.right)
	# calculate some kind of describing number for the two time windows
	windownumbers.left <- cbind(colSums(winone.left),colSums(wintwo.left))
	windownumbers.right <- cbind(colSums(winone.right),colSums(wintwo.right))
	# combine all into one matrix
	allwinnumbers <- cbind(windownumbers.left, windownumbers.right)
	allwinnumbers <- cbind(1:nrow(allwinnumbers),allwinnumbers)
	allwinnumbers <- data.frame(allwinnumbers)
	colnames(allwinnumbers) <- c("repetition", "leftwin1", "leftwin2", "rightwin1", "rightwin2")
	pdf()
	plot(x = allwinnumbers[, 1], y = allwinnumbers[, 2], type = "l", main = "First stimulation", lty = 2)
	lines(allwinnumbers[, 4], col = "blue", lty = 3)
	legend("bottomright", fill = c("black", "blue"), legend = c("left", "right"))
	plot(x = allwinnumbers[, 1], y = allwinnumbers[, 3], type = "l", main = "Last stimulation", lty = 2)
	lines(allwinnumbers[, 5], col = "blue", lty = 3)
	legend("bottomright", fill = c("black", "blue"), legend = c("left", "right"))
	# legend("bottomleft", fill = c("black", "blue", "orange", "red"), legend = c("leftwin1", "leftwin2", "rightwin1", "rightwin2"))
	dev.off()
	return(allwinnumbers)
}
