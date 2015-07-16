timer <- function(time.start = FALSE) {
	# a small timer function which returns the current time or the time difference to a time object
	if (time.start == FALSE) {
		return(Sys.time())
	}
	time.end <- Sys.time()
	timediff <- difftime(time.end, time.start, units = "secs")
	return(round(timediff))
}
