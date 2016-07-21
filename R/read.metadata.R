<<<<<<< HEAD:R/readmetadata.r
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
=======
#' read.metadata
#'
#' @description a function to read ephys metadata files.
#'
#' @param files the files metadata should be loaded for
>>>>>>> dev:R/read.metadata.R
#' @param datafolder where the data files are located
#' @param metafolder where the metadata files are located
#' @return returns a list with metadata for all files from the data folder
#' @export
read.metadata <- function(files, datafolder, metafolder) {
	# check whether the folder contains the folder meta with grepl(ogical)
# 	if (grepl("/meta", folder) == FALSE) {
# 		# try to change to the sub-folder meta
# 		result <- try(setwd(paste0(wd, "/meta")), silent = TRUE)
# 		# if it did not work stop and throw an error
# 		if (inherits(result, "try-error") == TRUE) {
# 			stop("Error: Wrong directory for readmetadata. Cannot find ›meta‹ folder!")
# 		}
# 	}

	# construct the metadata file paths
	metafiles <- paste0(metafolder, files)
	# check whether all metadata files exist
	if (all(file.exists(metafiles))) {
		# read in the metadata. metalist will be a list of data frames
		metalist <- parallel::mclapply(metafiles, readr::read_csv, col_names = FALSE)
	}
	else {
		writeLines("These data files do not have metadata:")
		writeLines(files[file.exists(metafiles) == FALSE])
		return(NULL)
	}
	# clean up the data—remove columns that only contain NA values if present
	metalist <- parallel::mclapply(metalist, Filter, f = function(x) {!all(is.na(x))})
	# change the column names of the data frames
	metalist <- parallel::mclapply(metalist, function(x) {colnames(x) <- c("parameter", "value"); x})
	# create datetime objects
	metalist <- parallel::mclapply(metalist, function(x) {strptime(x, format = "%F--%H-%M-%S"); x})
	return(metalist)
}
