create_png_overview <- function(wd = NA) {
	#' \code{create_png_overview} A function to create png overviews of ephys log files
	#' @param wd working directory where the raw data is
 
	# check whether a working directory was specified
	if (is.na("wd") == TRUE) {
		setwd("~/datarepos/ephys/data")
	}
	# get the files and read them
	files <- dir()
	data <- readephysdata(files, mc = TRUE)
	# separate data from filenames. filenames becomes a vector of filenames, rawdata becomes a list of all data frames containing the data
	filenames <- unlist(rawdata[1])
	data <- data[2]
	setwd("~/datarepos/ephys/png/")
	# plot the data from the files 
	for (i in 1:length(files)) {
		tsdata <- ts(data = data[i], start = 1/25000, end = 10, deltat = 1/25000)
		png(filename = sub(pattern = ".csv", replacement = ".png", x = files[i]))
		plot(tsdata)
		dev.off()
	}
}
