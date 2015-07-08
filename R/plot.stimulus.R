#' plot.stimulus
#' 
#' @description add stimuli to a plot. the stimuli data contain information about the position of the stimuli
#' @param stimdf a data frame containing stimulus data
plot.stimulus <- function(stimdf) {
	par(col = "darkgrey", axes = F, xlab = NA, ylab = NA)
	plot(stimdf$time, stimdf$leftbar, type = "l")
	plot(stimdf$time, stimdf$rightbar, type = "l")
	Axis(side = 1, col = "grey")
	mtext(side = 4, line = 3, "stimulus")
}
