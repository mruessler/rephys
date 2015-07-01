#' \code{batch_spikes} a first try on copying the functionality of the matlab toolbox batch_spikes.m function
#' more documentation might come in the future
#' steps: »unpack« the rawdata into filenames and data. find spikes in the data streams pack the data into one array
#'
#' @param rawdata ephys data to work on
#' @param std_factor depending on the quality of the data. A value of 1 is a good value for data with a really good signal to noise ratio.
#' @param min_isi minimum spike interval in ms.
#' @param sigma not yet implemented
#' @return processed spike data
batch_spikes <- function(rawdata, std_factor = 1, min_isi = 1, sigma = NA) {
	# get the filenames. filenames becomes a vector of filenames, rawdata becomes a list of all data frames containing the data
	filenames <- dir()
	#	do.call(cbind, rawdata)
	# preallocate the matrix
	# spikes <- matrix(data = NA, nrow = dim(rawdata[[1]][[1]])[1], ncol = length(filenames))
	# preallocate an array. each file is in one column, data values are in rows, and channels are in layers (z axis)
	spikes <- array(data = NA_real_, dim = dim(rawdata))

	# samples in ms – we take a shortcut here
	s_per_ms <- 25

	# main action loop
	for (i in 1:length(filenames)) {
		# select the recording column
		signal <- rawdata[, i]
		# filter the signal if sigma was specified
		if (is.na(sigma) == FALSE) {
			# TODO: sigma is not yet implemented
		}
		# compute threshold value; m and s have the dim(1, 2)
		m <- mean(signal)
		s <- sd(signal)

		thres <- m + s * std_factor
		# find spikes. send the data matrix in two parts–each vector individually
		rsp <- threshold_spikes(signal, thres, file = filenames[i])
		# spikes[,] <- prune_spikes()
		# fill the spikes matrix with data
		spikes[, i] <- rsp
	}
	print("fin")
	return(spikes)
}
