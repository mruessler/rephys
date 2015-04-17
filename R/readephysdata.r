readephysdata <- function(files, parallel = TRUE) {
	# a function to read ephys log files
	
	# load the required libraries
	library(parallel)
	# start the timer
	start.time <- timer()
	# create a list and fill it with the data
	if (parallel == TRUE) {
		ephyslist <- mclapply(files, read.csv, sep = ",", colClasses = "numeric")
	}
	else {
		ephyslist <- lapply(files, read.csv, sep = ",", colClasses = "numeric")
	}
	# time taken
	endtime <- timer()
	timediff <- timer(start.time)
	writeLines(paste("Reader took ", timediff, " seconds (", length(files), " files),", sep = ""))
	# return a list with the used files and the actual treadlist
	return(list(files, ephyslist))
}
