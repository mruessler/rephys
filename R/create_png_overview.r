create_png_overview <- function(wd = NA) {
	#' \code{create_png_overview} A function to create png overviews of ephys log files
	#' @param wd working directory where the data is. It is required to use the folder containing the data, meta, and png subfolders as input
 
	# check whether a working directory was specified
	if (is.na(wd) == TRUE) {
		stop("Error, no directory specified.")
	}
	else {
		dd <- paste(wd, "/data", sep = "")
	}
	# get the files from folder
	files <- dir(dd, pattern = ".csv")
	# get the data from the files
	data <- readephysdata(files, folder = dd, mc = TRUE)
	# plot the data from the files 
	print("Start writing png files")
	for (i in 1:length(files)) {
		tsdata <- ts(data = data[i], start = 1/25000, end = 10, deltat = 1/25000)
		png(filename = sub(pattern = ".csv", replacement = ".png", x = paste(wd, "/png/", files, sep = "")[i]))
		plot(tsdata, xlab = "Time (s)", ylab = "Amplitude", main = files[1])
		dev.off()
	}
}
