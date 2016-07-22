#' plot.raster
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
#' @description Create a raster plot with all the spike data
#' @param spikes spike vector
#' @param sr sampling rate
#' @export
#'
plot.raster <- function(spikes, sr) {
	# create the x axis
	xaxis <- (1:nrow(spikes)) / sr
	# create a row of dots for each column
	png(height = 100 * ncol(spikes), width = 1500)
	# par(mfrow = c(ncol(spikes), 1))
	spikings <- xaxis[spikes[, 1] > 0]
	plot(spikings, matrix(data = 1, nrow = 1, ncol = length(spikings)), type = "n")
	for (i in 1:ncol(spikes)) {
		spikings <- xaxis[spikes[, i] > 0]
		points(spikings, matrix(data = 1, nrow = 1, ncol = length(spikings)) * i, pch = ".")
	}
	dev.off()
}
