#' plot.stimulus
#'
#' @description add stimuli to a plot. the stimuli data contain information about the position of the stimuli
#' @param stimdf a data frame containing stimulus data
#' @param side select the side of which the stimulus is from
plot.stimulus <- function(stimdf, side = "left") {
	par(col = "black")
	if (side == "left") {
		plot(stimdf$time, stimdf$leftbar, type = "l", axes = F, xlab = NA, ylab = NA)
	}
	if (side == "right") {
		plot(stimdf$time, stimdf$rightbar, type = "l", axes = F, xlab = NA, ylab = NA)
		Axis(side = 1, col = "grey")
		mtext(side = 4, line = 3, "stimulus")
	}
}
