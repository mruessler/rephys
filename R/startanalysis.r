startanalysis <- function(mc = TRUE) {
	#' \code{startanalysis} This is the starting point of the analysis. The function sources some requisites, and sets the working directory (Details of this will change). The returned object contains all the data from the data directory. This object can then be handed over to the batch_spikes function for further analysis.
	#' @param mc enables or disables the use of multiple cores
	#' @return ephys raw data to be further processed

	wd <- getwd()
	source('~/src/workgroup/rtreadmillcode/timer.r')
	source('~/src/workgroup/rephys/R/get_H1signal.r')
	# temporarily function for testing purposes
	setwd("~/datarepos/ephys/data")
	# 	setwd("~/data/h1example")
	#  	setwd("~/data/h1artificial")
	# 	setwd("~/wolke/work/ephysdata/")
	files <- dir()
	files <- files[1:3]
	data <- readephysdata(files, mc)
	# 	setwd(wd)
	return(data)
}
