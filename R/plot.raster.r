#' plot.raster
#'
#' @param spikes 
#' @param sr sampling rate
#'
#' @export
#'
plot.raster <- function(spikes, sr) {
	# create the x axis
	xaxis <- (1:nrow(spikes)) / sr
	# create a row of dots for each column
	png(height = 100 * ncol(spikes), width = 1500)
	# par(mfrow = c(ncol(spikes), 1))
	spikings <- xaxis[spikes[, 1] > 0]
	plot(spikings, matrix(data = 1, nrow = 1, ncol = length(spikings)), type = "n")
	for (i in 1:ncol(spikes)) {
		spikings <- xaxis[spikes[, i] > 0]
		points(spikings, matrix(data = 1, nrow = 1, ncol = length(spikings)) * i, pch = ".")
	}
	dev.off()
}
