#' plot.png.overview
#'
#' \code{plot.png.overview} A function to create png overviews of ephys log files
#' @param wd working directory where the data is. It is required to use the folder containing the data, meta, and png subfolders as input
#' @param maxchunk numeric add a maximum number of files to be processed at once
#' @export
plot.png.overview <- function(wd = NA, maxchunk = 100) {
	# check whether a working directory was specified
	if (is.na(wd) == TRUE) {
		stop("Error, no directory specified.")
	}
	else {
		dd <- paste0(wd, "/data")
	}
	# get the files from folder
	storage <- dir(dd, pattern = ".csv")
	writeLines(paste(length(storage), "files to process."))
	# only do chunks of maxchunk at maximum (due to memory limitations)
	while (length(storage) > 0) {
		if (length(storage) >= maxchunk) {
			files <- storage[1:maxchunk]
		}
		else {
			files <- storage
		}
		# get the data from the files
		data <- read.ephysdata(files, folder = dd, mc = TRUE)
		# plot the data from the files
		writeLines("Start writing png files.")
		for (i in 1:length(files)) {
			# dual recordings
			if (ncol(data) == 2 * length(files)) {
				tsdata <- ts(data = data[((i * 2) - 1):(i * 2)], start = 1/25000, end = 10, deltat = 1/25000)
				png(width = 960, filename = sub(pattern = ".csv", replacement = "--raw.png", x = paste0(wd, "/png/", files)[i]))
				par(mfrow = c(2, 1), bty = "n")
				plot(tsdata[, 1], xlab = "", ylab = "Amplitude", xaxt = "n", main = files[i])
				plot(tsdata[, 2], xlab = "Time (s)", ylab = "Amplitude")
				dev.off()
			}
			# single recordings
			else {
				tsdata <- ts(data = data[i], start = 1/25000, end = 10, deltat = 1/25000)
				png(width = 960, filename = sub(pattern = ".csv", replacement = ".png", x = paste0(wd, "/png/", files)[i]))
				plot(tsdata, xlab = "Time (s)", ylab = "Amplitude", main = files[i])
				dev.off()
			}
		}
		writeLines("Finished writing png files.")
		storage <- storage[-(1:maxchunk)]
		# remove NA from storage if introduced
		storage <- storage[!is.na(storage)]
		if (length(storage) > 0) {
			writeLines(paste0(length(storage), " files left."))
		}
	}
}
