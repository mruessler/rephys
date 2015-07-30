#' batch.spikes
#'
#' \code{batch.spikes} a first try on copying the functionality of the matlab toolbox batch_spikes.m function
#' more documentation might come in the future
#' steps: »unpack« the rawdata into filenames and data. find spikes in the data streams pack the data into one array
#'
#' @param rawdata ephys data to work on
#' @param std.factor depending on the quality of the data. A value of 1 is a good value for data with a really good signal to noise ratio.
#' @param sigma not yet implemented
#' @return processed spike data
batch.spikes <- function(rawdata, std.factor = 1, sigma = NA) {
	# preallocate a data frame; each recorded channel is in one column, data values are in rows
	spikes <- matrix(data = NA_real_, nrow = nrow(rawdata), ncol = ncol(rawdata))

	# samples in ms – we take a shortcut here
	# s_per_ms <- 25

	# main action loop
	for (i in 1:ncol(spikes)) {
		# select the recording column
		signal <- rawdata[, i]
		# filter the signal if sigma was specified
		if (is.na(sigma) == FALSE) {
			# TODO: sigma is not yet implemented
		}
		# compute threshold value; m and s have the dim(1, 2)
		m <- mean(signal)
		s <- sd(signal)
		thres <- m + s * std.factor
		# find spikes. send the data matrix in two parts–each vector individually
		rsp <- threshold.spikes(signal, thres)
		# spikes[,] <- prune.spikes()
		# fill the spikes matrix with data
		spikes[, i] <- rsp
	}
	spikes <- data.frame(spikes)
	return(spikes)
}
