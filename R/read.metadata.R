#' read.metadata
#' 
#' @description a function to read ephys metadata files. 
#' 
#' @param datafolder where the data files are located
#' @param metafolder where the metadata files are located
#' @return returns a list with metadata for all files from the data folder
#' @export
read.metadata <- function(datafolder, metafolder) {
	# check whether the folder contains the folder meta with grepl(ogical)
# 	if (grepl("/meta", folder) == FALSE) {
# 		# try to change to the sub-folder meta
# 		result <- try(setwd(paste0(wd, "/meta")), silent = TRUE)
# 		# if it did not work stop and throw an error
# 		if (inherits(result, "try-error") == TRUE) {
# 			stop("Error: Wrong directory for readmetadata. Cannot find ›meta‹ folder!")
# 		}
# 	}
	# read in the list of data filenames
	datanames <- dir(path = datafolder, pattern = ".csv")
	# read in the list of metadata filenames
	metanames <- dir(path = metafolder, pattern = ".csv")
	# compare the filelists
	if (!identical(datanames, metanames)) {
		stop("There are differences between data and metadata filenames.")
	}
	# read in the metadata. metalist will be a list of data frames
	metalist <- parallel::mclapply(paste0(metafolder, metanames), read.csv, header = FALSE, sep = ",")
	# clean up the data—remove columns that only contain NA values if present
	metalist <- parallel::mclapply(metalist, Filter, f = function(x) {!all(is.na(x))})
	# change the column names of the data frames
	metalist <- parallel::mclapply(metalist, function(x) {colnames(x) <- c("parameter", "value"); x})
	# create datetime objects
	metalist <- parallel::mclapply(metalist, function(x) {strptime(x, format = "%F--%H-%M-%S"); x})
	return(metalist)
}
