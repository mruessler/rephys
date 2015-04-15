startanalysis <- function() {
	setwd("~/data/ephys/")
# 	setwd("~/wolke/work/ephysdata/")
	files <- dir()
	data <- readephysdata(files)
	return(data)
}

batch_spikes <- function(rawdata, std_factor = 1, min_isi = 1, sigma = NA) {
	# a first try on copying the functionality of the matlab toolbox batch_spikes.m function
	# more documentation might come in the future
	
	# separate data from filenames
	filenames <- unlist(data[1])
	rawdata <- rawdata[2]
	
	# preallocate the matrix
	# spikes <- matrix(data = NA, nrow = dim(rawdata[[1]][[1]])[1], ncol = length(filenames))
	# preallocate an array. each file is in one column, data values are in rows, and channels are in layers (z axis)
	spikes <- array(data = NA_real_, dim = c(dim(rawdata[[1]][[1]])[1], length(filenames), 2))
	
	# samples in ms – we take a shortcut here
	s_per_ms <- 25
	
	# main action loop
	for (i in 1:length(filenames)) {
		signal <- get_H1signal(rawdata[[1]][[i]])
		
		# filter the signal if sigma was specified
		if (is.na("sigma") == FALSE) {
			# not yet implemented
		}
		# compute threshold value; m and s have the dim(1, 2)
		m <- c(mean(signal[, 1]), mean(signal[, 2]))
		s <- c(sd(signal[, 1]), sd(signal[, 2]))
		thres1 <- m[1] + s[1] * std_factor
		thres2 <- m[2] + s[2] * std_factor
		
		# find spikes. send the data matrix in two parts–each vector individually
		rsp1 <- threshold_spikes(signal[, 1], thres1, file = filenames[i])
		rsp2 <- threshold_spikes(signal[, 2], thres2, file = filenames[i])
 		rsp <- cbind(rsp1, rsp2)
# 		spikes[,] <- prune_spikes()
	# fill the spikes matrix with data
	spikes[, i,] <- rsp
	}
	print("fin")
	return(spikes)
}
