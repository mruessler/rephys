#' plot.spikes
#'
#' @description plot spike data
#' @param spikes spike data
#' @param filelist list of files to plot
#' @param stimlist list of used stimuli
#' @param outputdir plot output direction
#'
#' @return nothing
#' @export
#'
plot.spikes <- function(spikes, filelist, stimlist, outputdir) {
	# plot the spikes and the stimuli
	for (i in 1:length(filelist)) {
		par(oma = c(rep(0, 4)), mar = c(1, 1, 1, 1), ps = 20, cex = 10)
		png(width = 1000, height = 750, filename = sub(pattern = ".csv", replacement = "--spikes.png", x = paste0(outputdir, filelist)[i]))
		layout(matrix(1:4, 4, 1), heights = c(rep(1/4, 4)))
		plot.trial(trial = spikes[, ((i * 2) - 1):(i * 2)], filename = filelist[i], stimuli = stimlist[[i]])
		dev.off()
	}
}
