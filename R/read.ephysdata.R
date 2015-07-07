#' read.ephysdata
#' 
#' @description a function to read ephys log files. 
#' @param files a list of filenames
#' @param folder character a folder path to read ephys data from
#' @param mc enables or disables the use of multiple cores
#' @return a data frame with ephys data
read.ephysdata <- function(files, folder, mc = TRUE) {
	# todo: add some checks for data validity (maybe filesize)
	
	# check whether the files vector does not contain "csv" and stop, if so
#   if (!(".csv" %in% files)) {
#   	print(getwd())
#   	print(files[1])
#   	stop("Error: Wrong vector of files given to readephysdata (no ›.csv‹ extension)!")
#   }
	# start the timer
	start.time <- timer()
	# create a list and fill it with the data
	# multi core variant
	if (mc == TRUE) {
		datalist <- parallel::mclapply(paste(folder, "/", files, sep = ""), read.csv, sep = ",", colClasses = "numeric")
	}
	else {
		datalist <- lapply(paste(folder, "/", files, sep = ""), read.csv, sep = ",", colClasses = "numeric")
	}
	# organize the data into a data frame
 	datadf <- do.call(cbind, datalist)
	if (ncol(datadf) == length(files) * 2) {
		print("Dual recording data detected")
		# adapt the vector of filenames to match the columns
		# first, double the elements
		# files <- as.vector(sapply(files, function (x) rep(x, 2)))
		# todo
		colnames(datadf) <- paste("c", 1:ncol(datadf), sep = "")
	}
	else {
		# use filenames as column names
		print("Single recording data detected")
		# colnames(datadf) <- files
		colnames(datadf) <- 1:ncol(datadf)
	}
	# time taken
	timediff <- timer(start.time)
	writeLines(paste("Reader took ", timediff, " seconds (", length(files), " files),", sep = ""))
	# return the data in a data frame. the filenames are the column names.
	return(datadf)
}
