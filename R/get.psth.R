#' get.psth
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
#' @description calculate a PSTH histogram
#' @param spikes a matrix of spike data
#' @param binwidth im ms
#' @param samplerate in Hertz
#'
#' @return psth data
#' @export
#'
get.psth <- function(spikes, binwidth = 10, samplerate = 25000) {
	# calculate samples per bin
	samplesperbin <- round(binwidth * samplerate / 1000)
	# calculate the number of bins
	n.bins <- floor(nrow(spikes) / samplesperbin)
	# preallocate a vector of length equal to the number of bins
	count <- vector("numeric", n.bins)
	# loop over bins to count spikes
	for (i in 1:n.bins) {
		bin <- spikes[(1 + (i - 1) * samplesperbin):(i * samplesperbin), ]
		# count the number of spikes
		count[i] <- sum(bin[,])
	}
	# normalise count by the number of trials
	normalcount <- count / ncol(spikes)
	binspersecond <- 1000 / binwidth
	# calculate psth: spikes / bin * bin / s
	psth <- normalcount * binspersecond
	return(psth)
}
