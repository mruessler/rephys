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

	# create a list and fill it with the data
	# multi core variant
	if (mc == TRUE) {
		datalist <- parallel::mclapply(paste0(folder, "/", files), readr::read_csv, col_names = FALSE, col_types = "dd")
	}
	else {
		datalist <- lapply(paste0(folder, "/", files), readr::read_csv, col_names = FALSE, col_types = "dd")
	}
	# organize the data into a data frame
	# TODO: check whether all elements have the same number of rows
 	datadf <- do.call(cbind, datalist)
 	# print information
	writeLines(paste0(length(files), " files loaded."))
	# return the data in a data frame. the filenames are the column names.
	return(datadf)
}
