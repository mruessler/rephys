#' batch_spikes
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
#' \code{batch_spikes} a first try on copying the functionality of the matlab toolbox batch_spikes.m function
#' more documentation might come in the future
#' steps: »unpack« the rawdata into filenames and data. find spikes in the data streams pack the data into one array
#'
#' @param rawdata ephys data to work on
#' @param std_factor depending on the quality of the data. A value of 1 is a good value for data with a really good signal to noise ratio.
#' @param min_isi minimum spike interval in ms.
#' @param sigma not yet implemented
#' @return processed spike data
batch_spikes <- function(rawdata, std_factor = 1, min_isi = 1, sigma = NA) {
	# get the filenames. filenames becomes a vector of filenames, rawdata becomes a list of all data frames containing the data
	# filenames <- dir()
	#	do.call(cbind, rawdata)
	# preallocate the matrix
	# spikes <- matrix(data = NA, nrow = dim(rawdata[[1]][[1]])[1], ncol = length(filenames))
	# preallocate an array. each file is in one column, data values are in rows, and channels are in layers (z axis)
	spikes <- array(data = NA_real_, dim = dim(rawdata))

	# samples in ms – we take a shortcut here
	s_per_ms <- 25

	# main action loop
	for (i in 1:ncol(spikes)) {
		# select the recording column
		signal <- rawdata[, i]
		# filter the signal if sigma was specified
		if (is.na(sigma) == FALSE) {
			# TODO: sigma is not yet implemented
		}
		# compute threshold value; m and s have the dim(1, 2)
		m <- mean(signal)
		s <- sd(signal)

		thres <- m + s * std_factor
		# find spikes. send the data matrix in two parts–each vector individually
		rsp <- threshold_spikes(signal, thres)
		# spikes[,] <- prune_spikes()
		# fill the spikes matrix with data
		spikes[, i] <- rsp
	}
	return(spikes)
}
