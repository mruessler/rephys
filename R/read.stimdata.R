#' read.stimdata
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
#' @description match the experiment parameters with a filename in the stimlibrary. the function takes the output of the readmetadata function as an input. from that data it constructs the file paths to the stimulus files. example filename from the library: ephyslog-amp25-avgpos100-freq0.5.log
#' @param metalist list of metadata data frames
#' @param stimlibrary character folder where the stim library is
#' @seealso readmetadata
#' @return a list with stimulus data frames
#' @export
#'
read.stimdata <- function(metalist, stimlibrary = "/home/martin/datarepos/ephysstimlibrary/") {
	# check if metalist does not contain NA data
	if (all(is.na(metalist[[1]]))) {
		stop("Metalist seems to be errorneous, stopping.")
	}
	# create an empty list
	stimlist <- list()
	# construct paths to filenames from the metadata and store them in a list
	for (i in 1:length(metalist)) {
		stimlist[[i]] <- paste0(stimlibrary, "ephyslog-amp", metalist[[i]][2, 2], "-avgpos", metalist[[i]][3, 2], "-freq", metalist[[i]][4, 2], ".log")
	}
	# transform the list into a vector
	stimvector <- as.vector(unlist(stimlist))
	# check whether all stimulus files exist
	if (!all(file.exists(stimvector))) {
		warning(paste0("The stimulus library does not contain these files:", "\n", stimvector[file.exists(stimvector) == FALSE]))
		return(NULL)
	}
	# load the stimulus data
	stimlist <- lapply(stimvector, readr::read_delim, delim = " ", col_names = FALSE, col_types = "c_c_c_c_")
	# add column names to the stimulus data
	stimlist <- parallel::mclapply(stimlist, function(x) {colnames(x) <- c("time", "unixtime", "leftbar", "rightbar"); x})
	# return the data
	return(stimlist)
}
