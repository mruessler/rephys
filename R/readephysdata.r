readephysdata <- function(files, mc = TRUE) {
	#' \code{readephysdata} a function to read ephys log files. 
	#' @param files a list of file names
	#' @param mc enables or disables the use of multiple cores
	#' @return a data frame with ephys data
	# todo: add some checks for data validity (maybe filesize)
	
	# load the required libraries
	library(parallel)
	library(signal)
	# start the timer
	start.time <- timer()
	# create a list and fill it with the data
	# multi core variant
	if (mc == TRUE) {
		ephyslist <- mclapply(files, read.csv, sep = ",", colClasses = "numeric")
	}
	else {
		ephyslist <- lapply(files, read.csv, sep = ",", colClasses = "numeric")
	}
	# organize the data into a data frame
 	ephysdf <- do.call(cbind, ephyslist)
	if (ncol(ephysdf) == length(files) * 2) {
		print("Dual recording data detected")
		# adapt the vector of filenames to match the columns
		# first, double the elements
		# files <- as.vector(sapply(files, function (x) rep(x,2)))
		# todo
	}
	else {
		# use filenames as column names
		print("Single recording data detected")
		# colnames(ephysdf) <- files
	}
	# time taken
	endtime <- timer()
	timediff <- timer(start.time)
	writeLines(paste("Reader took ", timediff, " seconds (", length(files), " files),", sep = ""))
	# return the data in a data frame. the filenames are the column names.
	return(ephysdf)
}
