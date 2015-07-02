#' startanalysis
#' @name startanalysis
#' @description This is the starting point of the analysis. The function sources some requisites, and sets the working directory (Details of this will change). The returned object contains all the data from the data directory. This object can then be handed over to the batch_spikes function for further analysis.
#' @param mc logical enables or disables the use of multiple cores
#' @return ephys raw data to be further processed
#' @export
startanalysis <- function(mc = TRUE) {
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
	data <- readephysdata(files, folder = dd, mc = mc)
	print("got the data")
	# transform data into spike data
	spikes <- batch_spikes(data)
	print("data transformed")
	# read metadata for the data
	metalist <- readmetadata(datafolder = dd, metafolder = md)
	print("metadata loaded")
	stimlist <- readstimdata(metalist = metalist, stimlibrary = sd)
	print("stimulus data loaded")
	# plot the spikes and the stimuli
	for (i in 1:ncol(spikes)) {
		png(file = paste(pd, "spike", i, ".png", sep = ""), width = 1500)
		par(mfrow = c(3, 1))
		plotspike(spikes[, i])
		plotstimulus(stimlist[[ceiling(i / 2)]])
		dev.off()
	}
	print("finished the run")
}
