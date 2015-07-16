#' autoanalysis
#'
#' @description This is the starting point of the analysis. The function will analyse ephys data with default parameters. The returned object contains all the data from the data directory. This object can then be handed over to the batch_spikes function for further analysis.
#' @param mc logical enables or disables the use of multiple cores
#' @return spikes processed spike data
#' @export
autoanalysis <- function(mc = TRUE) {
		# temporarily function for testing purposes
	# select a data dir, meta dir, stim dir
	# dd <- "~/datarepos/ephysfull/data"
	env <- "data"
	if (env == "wolke") {
		dd <- "~/wolke/work/ephys/data"
		md <- "~/wolke/work/ephys/meta/"
		sd <- "~/wolke/work/ephys/lib/"
		pd <- "~/wolke/work/ephys/png/"
	}
	else {
		dd <- "~/datarepos/ephys/data/"
		md <- "~/datarepos/ephys/meta/"
		sd <- "~/datarepos/ephysstimlibrary/"
		pd <- "~/datarepos/ephys/png/"
	}
	if (env == "data") {
		dd <- "~/datarepos/ephysfull/data/"
		md <- "~/datarepos/ephysfull/meta/"
		pd <- "~/datarepos/ephysfull/png/"
	}
	files <- dir(dd, pattern = ".csv")
	# reduce the amount of files for development
	dev <- TRUE
	if (dev == TRUE) {
		files <- files[(length(files) - 1):length(files)]
	}
	# get the data
	start.time <- timer()
	data <- read.ephysdata(files, folder = dd, mc = mc)
	time.diff <- timer(start.time)
	writeLines(paste0("Data loaded (", time.diff, " seconds)."))
	# transform data into spike data
	start.time <- timer()
	spikes <- batch.spikes(data, std.factor = 4)
	time.diff <- timer(start.time)
	writeLines(paste0("Data transformed to spike data (", time.diff, " seconds)."))
	# prune spikes
	start.time <- timer()
	spikes <- prune.spikes(spikes, min.isi = 75)
	time.diff <- timer(start.time)
	writeLines(paste0("Spikes pruned (", time.diff, " seconds)."))
	# plot.raster(spikes, 25000)
	# read metadata for the data
	start.time <- timer()
	metalist <- read.metadata(files = files, datafolder = dd, metafolder = md)
	time.diff <- timer(start.time)
	if (is.null(metalist)) {
		return("Something went wrong during metadata retrieval.")
	}
	writeLines(paste0("Metadata loaded (", time.diff, " seconds)."))
	start.time <- timer()
	stimlist <- read.stimdata(metalist = metalist, stimlibrary = sd)
	time.diff <- timer(start.time)
	if (is.null(stimlist)) {
		return("Something went wrong during stimulus data retrieval.")
	}
	writeLines(paste0("Stimulus data loaded (", time.diff, " seconds)."))
	# start plotting
	plot.data(spikes, stimlist, files, pd)
	writeLines("Finished autoprocessing.")
	return(spikes)
}

plot.data <- function(spikes, stimlist, files, pd) {
	# plot the spikes and the stimuli
	start.time <- timer()
	for (i in 1:length(files)) {
		png(width = 1500, filename = sub(pattern = ".csv", replacement = "--spikes.png", x = paste0(pd, files)[i]))
		par(mfrow = c(4, 1), bty = "n", cex = 1)
		tsspikes <- ts(data = spikes[((i * 2) - 1):(i * 2)], start = 1/25000, end = 10, deltat = 1/25000)
		print(head(tsspikes))
		plot.spikes(tsspikes[, 1], xlab = "", ylab = "Left channel", xaxt = "n", main = files[i])
		plot.spikes(tsspikes[, 2], xlab = "", ylab = "Right channel")
		plot.stimulus(stimlist[[i]])
		dev.off()
	}
	time.diff <- timer(start.time)
	writeLines(paste0("Plotting completed (", time.diff, " seconds)."))
}
