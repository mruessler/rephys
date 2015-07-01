#' \code{gauss_filter} this function applies a gaussian convolution filter to the input signal. the window width is specified by sigma
#' @param signal
#' @param sigma
gauss_filter <- function(signal, sigma) {
	# calculate window size
	alpha <- 2.5
	window_size <- round(sigma * alpha)
	# make sure window size is an odd number
	if (window_size %% 2 == 0) {
		window_size <- window_size + 1
	}
	# generate filter
	gauss_window <- signal::gausswin(window_size, alpha)
	# normalize the filter kernel
	gauss_window <- gauss_window / sum(gauss_window)
	# compute the signal mean
	signal_mean <- mean(signal)
	# convolve zero-mean signal
	signal_conv <- signal::conv(signal - signal_mean, gauss_window)
	# select valid section of the result and add mean
	signal_filtered <- signal_conv(((window_size - 1) / 2 + 1):nrow(signal_filtered) - (window_size - 1) / 2) + signal_mean
	return(signal_filtered)
}
