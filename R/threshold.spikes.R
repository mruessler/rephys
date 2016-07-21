<<<<<<< HEAD:R/threshold_spikes.r
#' threshold_spikes
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
#' \code{threshold_spikes} apply a threshold to the input signal and return a vector of 0 and 1
=======
#' threshold.spikes
#' 
#' \code{threshold.spikes} apply a threshold to the input signal and return a vector of 0 and 1
>>>>>>> dev:R/threshold.spikes.R
#' @param signal numeric vector of ephys data
#' @param threshold numeric value
#' @return a matrix with spikes
#' @export
threshold.spikes <- function(signal, threshold) {
	# check where the signal exceeds the threshold an convert the resulting vector from logical to numeric values of 0 and 1
	ts <- (signal > threshold) + 0
	# check whether ts consists only of zeros
	if (isTRUE(zero.range(ts))) {
		writeLines("Threshold is higher than any value from the input vector")
	}
	# look for start/end of continuous sections:
	# diff of a vector of logical values gives:
	#    1 if v(n - 1) = 0 and v(n) = 1
	#    0 if v(n - 1) == v(n)
	#   -1 if v(n - 1) = 1 and v(n) = 0
	starts <- which(diff(ts) > 0)# + 1
	ends <- which(diff(ts) < 0)# + 1

	# either starts or ends may no have any content. let’s check and stop in that case
	if (is.na(starts[1] > ends[1])) {
		writeLines("Error, start and end are not comparable. Returning empty vector.")
		spikes <- vector("numeric", length(signal))
		return(spikes)
	}
	# signal may start or end above threshold
	# if it starts above threshold add 1 to starts
	if (starts[1] > ends[1]) {
		starts <- c(1, starts)
	}
	# if it ends above threshold add the last element to ends
	if (length(starts) > length(ends)) {
		ends <- c(ends, length(signal))
	}
	# preallocate spike vector
	spikes <- vector("numeric", length(signal))
	# create segments of equal value
	for (i in 1:length(starts)) {
		segment <- signal[starts[i]:ends[i]]
		# get the position of max value
		pos <- which.max(segment)
		spikes[starts[i] + pos - 1] <- 1
	}
	return(spikes)
}
