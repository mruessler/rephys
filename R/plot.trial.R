plot.trial <- function(trial, filename, stimuli = NULL) {
	tstrial <- ts(data = trial, start = 1/25000, end = 10, deltat = 1/25000)
	plot(tstrial[, 1], type = "l", axes = FALSE, frame.plot = FALSE, xlab = "", ylab = "Left channel", xaxt = "n", main = filename)
	plot(tstrial[, 2], type = "l", axes = FALSE, frame.plot = FALSE, xlab = "", ylab = "Right channel")
	# only plot stimuli if requested
	if (!is.null(stimuli)) {
		plot.stimulus(stimuli, side = "left")
		plot.stimulus(stimuli, side = "right")
	}
}
