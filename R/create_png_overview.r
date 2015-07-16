create_png_overview <- function(wd = NA) {
	#' \code{create_png_overview} A function to create png overviews of ephys log files
	#' @param wd working directory where the data is. It is best to use the folder containing the data, meta, and png subfolders as input
 
	# check whether a working directory was specified
	if (is.na(wd) == TRUE) {
		setwd("~/datarepos/ephys/data")
		wd <- "~/datarepos/ephys"
	}
	else {
		setwd(paste(wd, "/data", sep = ""))
	}
	# get the files from folder
	files <- dir()
	# get the data from the files
	data <- readephysdata(files, mc = TRUE)
	# change the working directory to the png sub-folder
	setwd(paste(wd, "/png", sep = ""))
	# plot the data from the files 
	print("Start writing png files")
	for (i in 1:length(files)) {
		tsdata <- ts(data = data[i], start = 1/25000, end = 10, deltat = 1/25000)
		png(filename = sub(pattern = ".csv", replacement = ".png", x = files[i]))
		plot(tsdata, xlab = "Time (s)", ylab = "Amplitude", main = files[1])
		dev.off()
	}
	# set the working directory to the containing folder
	setwd(wd)
}
