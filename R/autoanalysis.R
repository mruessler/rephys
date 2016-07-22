#' autoanalysis
#'
#' This file is part of the rephys package.
#'
#' Copyright(c) Martin Rüßler
#'
#' This file may be licensed under the terms of of the
#' GNU General Public License Version 3 (the ``GPL''),
#' or (at your option) any later version.
#'
#' Software distributed under the License is distributed
#' on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY KIND, either
#' express or implied. See the GPL for the specific language
#' governing rights and limitations.
#'
#' You should have received a copy of the GPL along with this
#' program. If not, go to http://www.gnu.org/licenses/gpl.html
#' or write to the Free Software Foundation, Inc.,
#' 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#'
#' The development of this software was supported by the
#' Excellence Cluster EXC 277 Cognitive Interaction Technology.
#' The Excellence Cluster EXC 277 is a grant of the Deutsche
#' Forschungsgemeinschaft (DFG) in the context of the German
#' Excellence Initiative.
#'
#' @description This is the starting point of the analysis. The function will analyse ephys data with default parameters. The returned object contains all the data from the data directory. This object can then be handed over to the batch_spikes function for further analysis.
#' @param mc logical enables or disables the use of multiple cores
#' @return spikes processed spike data
#' @export
#'
autoanalysis <- function(mc = TRUE, env = NULL) {
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
		dd <- "~/datarepos/2016-05-20/data/"
		md <- "~/datarepos/2016-05-20/meta/"
		pd <- "~/datarepos/2016-05-20/png/"
	}
	files <- dir(dd, pattern = ".csv")
	# reduce the amount of files for development
	dev <- TRUE
	if (dev == TRUE) {
		files <- files[(length(files) - 11):length(files)]
	}
	# get the data
	start.time <- timer()
	data <- read.ephysdata(files, folder = dd, mc = mc)
	time.diff <- timer(start.time)
	writeLines(paste0("Data loaded (", time.diff, " seconds)."))
	gc()
	# transform data into spike data
	start.time <- timer()
	spikes <- batch.spikes(data, std.factor = 3)
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
	# plot.data(spikes = spikes, filelist = files, stimlist = stimlist, outputdir = pd)
	plot.spikes(spikes = spikes, filelist = files, stimlist = stimlist, outputdir = pd)
	time.diff <- timer(start.time)
	writeLines(paste0("Spike data plotted (", time.diff, " seconds)."))
	writeLines("Finished autoprocessing.")
	return(spikes)
}
