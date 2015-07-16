#' timer
#'
#' @param time.start a time object
#' @description can be called both with and without a time point
#' @return eiter a time or a time difference depending on how the function was called
#' @export
#'
#' @examples
#' function.start <- timer()
#' Sys.sleep(3)
#' time.diff <- timer(function.start)
#' print(paste("Function took", time.diff, "seconds."))
timer <- function(time.start = FALSE) {
	# a small timer function which returns the current time or the time difference to a time object
	if (time.start == FALSE) {
		return(Sys.time())
	}
	time.end <- Sys.time()
	timediff <- difftime(time.end, time.start, units = "secs")
	return(round(timediff))
}
