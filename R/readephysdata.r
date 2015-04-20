readephysdata <- function(files, mc = TRUE) {
	# a function to read ephys log files
	
	# load the required libraries
	library(parallel)
	library(signal)
	# start the timer
	start.time <- timer()
	# create a list and fill it with the data
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
	}
	colnames(ephysdf) <- files
	# time taken
	endtime <- timer()
	timediff <- timer(start.time)
	writeLines(paste("Reader took ", timediff, " seconds (", length(files), " files),", sep = ""))
	# return the data in a data frame. the filenames are the column names.
	return(ephysdf)
}
