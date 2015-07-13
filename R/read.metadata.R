#' read.metadata
#' 
#' @description a function to read ephys metadata files. 
#' 
#' @param files the files metadata should be loaded for
#' @param datafolder where the data files are located
#' @param metafolder where the metadata files are located
#' @return returns a list with metadata for all files from the data folder
#' @export
read.metadata <- function(files, datafolder, metafolder) {
	# check whether the folder contains the folder meta with grepl(ogical)
# 	if (grepl("/meta", folder) == FALSE) {
# 		# try to change to the sub-folder meta
# 		result <- try(setwd(paste0(wd, "/meta")), silent = TRUE)
# 		# if it did not work stop and throw an error
# 		if (inherits(result, "try-error") == TRUE) {
# 			stop("Error: Wrong directory for readmetadata. Cannot find ›meta‹ folder!")
# 		}
# 	}
	
	# construct the metadata file paths
	metafiles <- paste0(metafolder, files)
	# check whether all metadata files exist
	if (all(file.exists(metafiles))) {
		# read in the metadata. metalist will be a list of data frames
		metalist <- parallel::mclapply(metafiles, read.csv, header = FALSE, sep = ",")
	}
	else {
		writeLines("These data files do not have metadata:")
		writeLines(files[file.exists(metafiles) == FALSE])
		return(NULL)
	}
	# clean up the data—remove columns that only contain NA values if present
	metalist <- parallel::mclapply(metalist, Filter, f = function(x) {!all(is.na(x))})
	# change the column names of the data frames
	metalist <- parallel::mclapply(metalist, function(x) {colnames(x) <- c("parameter", "value"); x})
	# create datetime objects
	metalist <- parallel::mclapply(metalist, function(x) {strptime(x, format = "%F--%H-%M-%S"); x})
	return(metalist)
}
