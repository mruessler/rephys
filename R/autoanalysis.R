#' autoanalysis
#'
#' @description This is the starting point of the analysis. The function will analyse ephys data with default parameters. The returned object contains all the data from the data directory. This object can then be handed over to the batch_spikes function for further analysis.
#' @param mc logical enables or disables the use of multiple cores
#' @return ephys raw data to be further processed
#' @export
autoanalysis <- function(mc = TRUE) {
		# temporarily function for testing purposes
	# select a data dir, meta dir, stim dir
	# dd <- "~/datarepos/ephysfull/data"
	env <- "data"
	if (env == "wolke") {
		dd <- "~/wolke/work/ephys/data/"
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
	files <- dir(dd)
	# get the data
	data <- read.ephysdata(files, folder = dd, mc = mc)
	print("got the data")
	# transform data into spike data
	spikes <- batch.spikes(data, std.factor = 4)
	print("data transformed to spike data")
	# prune spikes
	spikes <- prune.spikes(spikes, min.isi = 75)
	print("spikes pruned")
	# plot.raster(spikes, 25000)
	# read metadata for the data
	metalist <- read.metadata(datafolder = dd, metafolder = md)
	print("metadata loaded")
	stimlist <- read.stimdata(metalist = metalist, stimlibrary = sd)
	print("stimulus data loaded")
	# plot the spikes and the stimuli
	for (i in 1:ncol(spikes)) {
		png(file = paste(pd, "spike", i, ".png", sep = ""), width = 1500)
		par(mfrow = c(3, 1))
		plot.spikes(spikes[, i])
		plot.stimulus(stimlist[[ceiling(i / 2)]])
		dev.off()
	}
	print("finished the run")
}
