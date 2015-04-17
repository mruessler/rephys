gauss_filter <- function(signal, sigma) {
	# this function applies a gaussian convolution filter to the input signal. the window width is specified by sigma
	
	# calculate window size
	alpha <- 2.5
	window_size <- round(sigma * alpha)
	# make sure window size is an odd number
	if (window_size %% 2 == 0) {
		window_size <- window_size + 1
	}
	# generate filter
	gauss_win <- 
	 
# 	 % generate filter
# 	 gwin=gausswin(winsize, alpha);
# 	 
# 	 % normalize filter kernel
# 	 gwin=gwin./sum(gwin);
# 	 
# 	 % compute signal mean
# 	 signal_mean=mean(signal);
# 	 
# 	 % convolve zero-mean signal
# 	 conv_result=conv(signal-signal_mean, gwin);
# 	 
# 	 % select valid section of the result and add mean
# 	 filtered_signal=...
# 	 conv_result((winsize-1)/2+1:end-(winsize-1)/2)+signal_mean;
	 
	return(filtered_signal)
}
