#' plot.trial
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
#' @description plot an experimental trial
#' @param filename filename
#' @param filelist list of files to plot
#' @param stimuli plot stimuli
#' @return nothing
#' @export
#'
plot.trial <- function(trial, filename, stimuli = NULL) {
	tstrial <- ts(data = trial, start = 1/25000, end = 10, deltat = 1/25000)
	plot(tstrial[, 1], type = "l", axes = FALSE, frame.plot = FALSE, xlab = "", ylab = "Left channel", xaxt = "n", main = filename)
	plot(tstrial[, 2], type = "l", axes = FALSE, frame.plot = FALSE, xlab = "", ylab = "Right channel")
	# only plot stimuli if requested
	if (!is.null(stimuli)) {
		plot.stimulus(stimuli, side = "left")
		plot.stimulus(stimuli, side = "right")
	}
}
