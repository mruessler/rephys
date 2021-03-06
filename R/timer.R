#' timer
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
#' @param time.start a time object
#' @description can be called both with and without a time point
#' @return eiter a time or a time difference depending on how the function was called
#' @export
#'
#' @examples
#' function.start <- timer()
#' Sys.sleep(3)
#' time.diff <- timer(function.start)
#' print(paste("Function took", time.diff, "seconds."))
#'
timer <- function(time.start = FALSE) {
	# a small timer function which returns the current time or the time difference to a time object
	if (time.start == FALSE) {
		return(Sys.time())
	}
	time.end <- Sys.time()
	timediff <- difftime(time.end, time.start, units = "secs")
	return(round(timediff))
}
