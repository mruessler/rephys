plot.spiketrain <- function(spike, col = "black", ...) {
	# convenience function for single spike train plotting
	plot(spike, type = "p", ylim = c(0.9, 1.1), axes = FALSE, pch = ".", frame.plot = FALSE, col = col, ...)
	# Axis(side = 1, col = "grey")
}
