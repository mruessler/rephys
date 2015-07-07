#' plot.stimulus
#' 
#' @description add stimuli to a plot. the stimuli data contain information about the position of the stimuli
#' @param stimdf a data frame containing stimulus data
plot.stimulus <- function(stimdf) {
	plot(stimdf$time, stimdf$leftbar, type = "l", col = "darkgrey", axes = F, xlab = NA, ylab = NA)
	plot(stimdf$time, stimdf$rightbar, type = "l", col = "darkgrey", axes = F, xlab = NA, ylab = NA)
	mtext(side = 4, line = 3, "stimulus")
}
