startanalysis <- function(mc = TRUE) {
	#' \code{startanalysis} This is the starting point of the analysis. The function sources some requisites, and sets the working directory (Details of this will change). The returned object contains all the data from the data directory. This object can then be handed over to the batch_spikes function for further analysis.
	#' @param mc enables or disables the use of multiple cores
	#' @return ephys raw data to be further processed

	wd <- getwd()
	# load all the scripts
	scripts <- paste("R/", dir("R", pattern = "*.r$"), sep = "")
	if (length(scripts) > 1) {
		sapply(scripts, source, .GlobalEnv)
	}
	# temporarily function for testing purposes
	# setwd("~/datarepos/ephys/data")
	# 	setwd("~/data/h1example")
	#  	setwd("~/data/h1artificial")
	setwd("~/wolke/work/ephys/data")
	files <- dir()
	data <- readephysdata(files, mc)
	# 	setwd(wd)
	return(data)
}
