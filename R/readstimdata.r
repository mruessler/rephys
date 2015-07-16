readstimdata <- function(metalist, stimlibrary = "/home/martin/datarepos/ephysstimlibrary/") {
	#' @name readstimdata
	#'
	#' @description match the experiment parameters with a filename in the stimlibrary. the function takes the output of the readmetadata function as an input. from that data it constructs the file paths to the stimulus files. example filename from the library: ephyslog-amp25-avgpos100-freq0.5.log
	#' @param metalist list of metadata data frames
	#' @param stimlibrary character folder where the stim library is
	#' @seealso readmetadata
	#' @return a list with stimulus data frames
	#' @export
	#'
	#' @examples
	
	# create an empty list
	stimlist <- list()
	# construct paths to filenames from the metadata and store them in a list
	for (i in 1:length(metalist)) {
		stimlist[[i]] <- paste(stimlibrary, "ephyslog-amp", metalist[[i]][2, 2], "-avgpos", metalist[[i]][3, 2], "-freq", metalist[[i]][4, 2], ".log", sep = "")
	}
	# transform the list into a vector
	stimvector <- as.vector(unlist(stimlist))
	# load the stimulus data
	stimlist <- mclapply(stimvector, read.csv, sep = " ", header = FALSE, colClasses = "numeric")
	# remove empty columns
	stimlist <- mclapply(stimlist, Filter, f = function(x) {!all(is.na(x))})
	# add column names to the stimulus data
	stimlist <- mclapply(stimlist, function(x) {colnames(x) <- c("time", "unixtime", "leftbar", "rightbar"); x})
	# return the data
	return(stimlist)
}
