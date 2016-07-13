#' readmetadata
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
#' @description a function to read ephys metadata files.
#' @param datafolder where the data files are located
#' @param metafolder where the metadata files are located
#' @return returns a list with metadata for all files from the data folder
#' @export
readmetadata <- function(datafolder, metafolder) {
	# check whether the folder contains the folder meta with grepl(ogical)
# 	if (grepl("/meta", folder) == FALSE) {
# 		# try to change to the sub-folder meta
# 		result <- try(setwd(paste(wd, "/meta", sep = "")), silent = TRUE)
# 		# if it did not work stop and throw an error
# 		if (inherits(result, "try-error") == TRUE) {
# 			stop("Error: Wrong directory for readmetadata. Cannot find ›meta‹ folder!")
# 		}
# 	}
	# read in the list of data file names
	datanames <- dir(path = datafolder, pattern = ".csv")
	# read in the list of metadata file names
	metanames <- dir(path = metafolder, pattern = ".csv")
	# compare the filelists
	if (!identical(datanames, metanames)) {
		stop("There are differences between data and metadata.")
	}
	# read in the metadata. metalist will be a list of data frames
	metalist <- parallel::mclapply(paste(metafolder, metanames, sep = ""), read.csv, header = FALSE, sep = ",")
	# clean up the data—remove columns that only contain NA values if present
	metalist <- parallel::mclapply(metalist, Filter, f = function(x) {!all(is.na(x))})
	# change the column names of the data frames
	metalist <- parallel::mclapply(metalist, function(x) {colnames(x) <- c("parameter", "value"); x})
	# create datetime objects
	metalist <- parallel::mclapply(metalist, function(x) {strptime(x, format = "%F--%H-%M-%S"); x})
	return(metalist)
}
