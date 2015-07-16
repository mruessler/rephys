startanalysis <- function(mc = TRUE) {
	#' \code{startanalysis} This is the starting point of the analysis. The function sources some requisites, and sets the working directory (Details of this will change). The returned object contains all the data from the data directory. This object can then be handed over to the batch_spikes function for further analysis.
	#' @param mc enables or disables the use of multiple cores
	#' @return ephys raw data to be further processed

	# temporarily function for testing purposes
	# setwd("~/datarepos/ephys/data")
	# 	setwd("~/data/h1example")
	#  	setwd("~/data/h1artificial")
	# select a data dir
	dd <- "~/wolke/work/ephys/data"
	files <- dir(dd)
	data <- readephysdata(files, mc)
	return(data)
}
