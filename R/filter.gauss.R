#' filter.gauss
#' 
#' \code{filter.gauss} this function applies a gaussian convolution filter to the input signal. the window width is specified by sigma
#' @param signal
#' @param sigma
filter.gauss <- function(signal, sigma) {
	# calculate window size
	alpha <- 2.5
	window.size <- round(sigma * alpha)
	# make sure window size is an odd number
	if (window.size %% 2 == 0) {
		window.size <- window.size + 1
	}
	# generate filter
	gauss.window <- signal::gausswin(window.size, alpha)
	# normalize the filter kernel
	gauss.window <- gauss.window / sum(gauss.window)
	# compute the signal mean
	signal.mean <- mean(signal)
	# convolve zero-mean signal
	signal.conv <- signal::conv(signal - signal.mean, gauss.window)
	# select valid section of the result and add mean
	signal.filtered <- signal.conv(((window.size - 1) / 2 + 1):nrow(signal.filtered) - (window.size - 1) / 2) + signal.mean
	return(signal.filtered)
}
