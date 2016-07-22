#' get.isipsth
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
#' @description calculate an inter-spike-interval histogram
#' @param spikes matrix of spike data
#' @param binwidth in milliseconds
#' @param samplerate in Hertz
#'
#' @return isipsth matrix
#' @export
#'
get.isipsth <- function(spikes, binwidth = 10, samplerate = 25000) {
	# preallocate a matrix
	freq.estimates <- matrix(data = NA, nrow = nrow(spikes), ncol = ncol(spikes))
	# loop through the columns
	for (i in 1:ncol(spikes)) {
		# get the indices of non-zero elements of the current column
		spikestimes <- which(spikes[, i] != 0)
		# check whether there are actual spikes in the current column
		if (length(spikestimes) > 0) {
			for (j in 2:length(spikestimes)) {
				# get the inter-spike-interval between this and the previous spike
				isi <- spikestimes[j] - spikestimes[j - 1]
				# apply the estimated frequency to all values from the bin
				freq.estimates[(spikestimes[j - 1] + 1):spikestimes[j], i] <- samplerate / isi
			}
			freq.estimates[1:spikestimes[1], i] <- freq.estimates[spikestimes[2], i]
			if (spikestimes[length(spikestimes)] < ncol(freq.estimates)) {
				freq.estimates[(spikestimes[length(spikestimes)] + 1):nrow(freq.estimates), i] <- freq.estimates[spikestimes[length(spikestimes)], i]
			}
		}
	}
	# change NA values to zero
	freq.estimates[!is.finite(freq.estimates)] <- 0
	mean.frequency <- rowMeans(freq.estimates)
	# bin the data
	if (binwidth > 0) {
		samplesperbin <- round(binwidth * samplerate / 1000)
		n.bins <- floor((nrow(spikes)) / samplesperbin)
		# calculate the psth as mean of the bins: transform the frequency vector to n columns with samplesperbin rows and compute the mean of columns
		psth <- colMeans(matrix(mean.frequency[1:(n.bins * samplesperbin)], nrow = samplesperbin, ncol = n.bins))
	}
	else {
		psth <- mean.frequency
	}
	return(psth)
}
