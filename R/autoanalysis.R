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
	env <- "wolke"
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
	spikes <- prunespikes(as.matrix(spikes), minisi = 75)
	time.diff <- timer(start.time)
	writeLines(paste0("Spikes pruned (", time.diff, " seconds)."))
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
	start.time <- timer()
	plot.data(spikes = spikes, filelist = files, stimlist = stimlist, outputdir = pd)
	time.diff <- timer(start.time)
	writeLines(paste0("Spike data plotted (", time.diff, " seconds)."))
	writeLines("Finished autoprocessing.")
	return(spikes)
}
