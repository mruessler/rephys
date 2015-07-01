#' plotstimulus
#' 
#' @description add stimuli to a plot. the stimuli data contain information about the position of the stimuli
#' @param stimdf a data frame containing stimulus data
plotstimulus <- function(stimdf) {
	par(new = T)
	plot(stimdf$time, stimdf$leftbar, col = "grey", axes = F, xlab = NA, ylab = NA)
	axis(side = 4)
	mtext(side = 4, line = 3, "stimulus")
}
