#' plotstimulus
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
#' @description add stimuli to a plot. the stimuli data contain information about the position of the stimuli
#' @param stimdf a data frame containing stimulus data
plotstimulus <- function(stimdf) {
	plot(stimdf$time, stimdf$leftbar, type = "l", col = "darkgrey", axes = F, xlab = NA, ylab = NA)
	plot(stimdf$time, stimdf$rightbar, type = "l", col = "darkgrey", axes = F, xlab = NA, ylab = NA)
	mtext(side = 4, line = 3, "stimulus")
}
