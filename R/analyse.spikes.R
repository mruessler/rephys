#' analyse.spikes
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
#' @param spikes data frame of spike data
#'
#' @return allwinnumbers descriptive values in a data frame
#' @export
#'
analyse.spikes <- function(spikes) {
	# devtools::load_all();spikes <- autoanalysis()
	spikes.left <- spikes[, seq(1, ncol(spikes) - 1, 2)]
	spikes.right <- spikes[, seq(2, ncol(spikes), 2)]
	rm(spikes)
	# set samplerate
	rate <- 25000
	# 2 time windows with borders a/b and c/d
	a <- 0.25 * rate
	b <- 0.75 * rate
	c <- 8.25 * rate
	d <- 8.75 * rate
	# add rownumber
	#spikes <- cbind(1:nrow(spikes), spikes)
	# select the two windows
	winone.left <- spikes.left[a:b,]
	winone.right <- spikes.right[a:b,]

	wintwo.left <- spikes.left[c:d,]
	wintwo.right <- spikes.right[c:d,]

	rm(spikes.left)
	rm(spikes.right)
	# calculate some kind of describing number for the two time windows
	windownumbers.left <- cbind(colSums(winone.left),colSums(wintwo.left))
	windownumbers.right <- cbind(colSums(winone.right),colSums(wintwo.right))
	# combine all into one matrix
	allwinnumbers <- cbind(windownumbers.left, windownumbers.right)
	allwinnumbers <- cbind(1:nrow(allwinnumbers),allwinnumbers)
	allwinnumbers <- data.frame(allwinnumbers)
	colnames(allwinnumbers) <- c("repetition", "leftwin1", "leftwin2", "rightwin1", "rightwin2")
	pdf()
	plot(x = allwinnumbers[, 1], y = allwinnumbers[, 2], type = "l", main = "First stimulation", lty = 2)
	lines(allwinnumbers[, 4], col = "blue", lty = 3)
	legend("bottomright", fill = c("black", "blue"), legend = c("left", "right"))
	plot(x = allwinnumbers[, 1], y = allwinnumbers[, 3], type = "l", main = "Last stimulation", lty = 2)
	lines(allwinnumbers[, 5], col = "blue", lty = 3)
	legend("bottomright", fill = c("black", "blue"), legend = c("left", "right"))
	# legend("bottomleft", fill = c("black", "blue", "orange", "red"), legend = c("leftwin1", "leftwin2", "rightwin1", "rightwin2"))
	dev.off()
	return(allwinnumbers)
}
