#' plot.spikes
#'
#' @param spikes
#' @param filelist
#' @param stimlist
#' @param outputdir
#'
#' @return nothing
#' @export
#'
plot.spikes <- function(spikes, filelist, stimlist, outputdir) {
	# plot the spikes and the stimuli
	for (i in 1:length(filelist)) {
		par(oma = c(rep(0, 4)), ps = 20, cex = 10)
		png(width = 1000, height = 750, filename = sub(pattern = ".csv", replacement = "--spikes.png", x = paste0(outputdir, filelist)[i]))
		layout(matrix(1:4, 4, 1), heights = c(rep(1/4, 4)))
		tsspikes <- ts(data = spikes[, ((i * 2) - 1):(i * 2)], start = 1/25000, end = 10, deltat = 1/25000)
		plot.spiketrain(tsspikes[, 1], xlab = "", ylab = "Left channel", xaxt = "n", main = filelist[i])
		plot.spiketrain(tsspikes[, 2], xlab = "", ylab = "Right channel")
		plot.stimulus(stimlist[[i]], side = "left")
		plot.stimulus(stimlist[[i]], side = "right")
		dev.off()
	}
}
