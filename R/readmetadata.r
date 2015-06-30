readmetadata <- function(datafolder, metafolder) {
	#' @name \code{readmetadata} a function to read ephys metadata files. 
	#' 
	#' @param datafolder where the data files are located
	#' @param metafolder where the metadata files are located
	#' @return returns a list with metadata for all files from the data folder
	#' @export

	# check whether the folder contains the folder meta with grepl(ogical)
# 	if (grepl("/meta", folder) == FALSE) {
# 		# try to change to the sub-folder meta
# 		result <- try(setwd(paste(wd, "/meta", sep = "")), silent = TRUE)
# 		# if it did not work stop and throw an error
# 		if (inherits(result, "try-error") == TRUE) {
# 			stop("Error: Wrong directory for readmetadata. Cannot find ›meta‹ folder!")
# 		}
# 	}
	# read in the list of data file names
	datanames <- dir(path = datafolder, pattern = ".csv")
	# read in the list of metadata file names
	metanames <- dir(path = metafolder, pattern = ".csv")
	# compare the filelists
	if (!identical(datanames, metanames)) {
		stop("There are differences between data and metadata.")
	}
	# read in the metadata. metalist will be a list of data frames
	metalist <- mclapply(paste(metafolder, metanames, sep = ""), read.csv, header = FALSE, sep = ",")
	# clean up the data—remove columns that only contain NA values if present
	metalist <- mclapply(metalist, Filter, f = function(x) {!all(is.na(x))})
	# change the column names of the data frames
	metalist <- mclapply(metalist, function(x) {colnames(x) <- c("parameter", "value"); x})
	# create datetime objects
	metalist <- mclapply(metalist, function(x) {strptime(x, format = "%F--%H-%M-%S"); x})
	return(metalist)
}
