#' plot.spikes
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
#' @description plot spike data
#' @param spikes spike data
#' @param filelist list of files to plot
#' @param stimlist list of used stimuli
#' @param outputdir plot output direction
#'
#' @return nothing
#' @export
#'
plot.spikes <- function(spikes, filelist, stimlist, outputdir) {
	# plot the spikes and the stimuli
	for (i in 1:(length(filelist) / 2)) {
		par(oma = c(rep(0, 4)), mar = c(1, 1, 1, 1), ps = 20, cex = 10)
		png(width = 1000, height = 750, filename = sub(pattern = ".csv", replacement = "--spikes.png", x = paste0(outputdir, filelist)[i]))
		layout(matrix(1:4, 4, 1), heights = c(rep(1/4, 4)))
		result <- try(plot.trial(trial = spikes[, ((i * 2) - 1):(i * 2)], filename = filelist[i], stimuli = stimlist[[i]]), silent = TRUE)
		if (inherits(result, "try-error") == TRUE) {
			stop(paste("Spike plotting function experienced an error:", geterrmessage()))
		}
		dev.off()
	}
}
