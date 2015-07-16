readmetadata <- function(files, wd) {
	#' \code{readmetadata} a function to read ephys metadata files. 
	#' 
	#' @param files list of filenames to read in
	#' @param wd working directory
	#' @return returns a list with metadata

	# check whether the working directory contains the folder meta with grepl(ogical)
	if (grepl("/meta", wd) == FALSE) {
		# try to change to the sub-folder meta
		result <- try(setwd(paste(wd, "/meta", sep = "")), silent = TRUE)
		# _if it did not work stop and throw an error
		if (inherits(result, "try-error") == TRUE) {
			stop("Error: Wrong directory for readmetadata. Cannot find ›meta‹ folder!")
		}
	}
	# read in the metadata. metalist will be a list of data frames
	metalist <- mclapply(files, read.csv, header = FALSE, sep = ",")
	# clean up the data—remove columns that only contain NA values if present
	metalist <- lapply(metalist, Filter, f = function(x){!all(is.na(x))})
	# change the column names of the data frames
	
	# create datetime objects
	
	# match the experiment parameters with a filename in the stimlibrary
	# example filename from the library: ephyslog-amp25-avgpos100-freq0.5
	
}
