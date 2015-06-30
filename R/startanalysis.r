startanalysis <- function(mc = TRUE) {
	#' startanalysis
	#' @name startanalysis
	#' @description This is the starting point of the analysis. The function sources some requisites, and sets the working directory (Details of this will change). The returned object contains all the data from the data directory. This object can then be handed over to the batch_spikes function for further analysis.
	#' @param mc logical enables or disables the use of multiple cores
	#' @return ephys raw data to be further processed
	#' @export

	# temporarily function for testing purposes
	# select a data dir
	dd <- "~/datarepos/ephysfull/data"
	dd <- "~/datarepos/ephys/data"
	# 	setwd("~/data/h1example")
	# dd <- "~/wolke/work/ephys/data"
	files <- dir(dd)
	data <- readephysdata(files, folder = dd, mc = mc)
	return(data)
}
