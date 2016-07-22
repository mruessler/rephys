#' zero.range
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
#' @description Determine if range of vector is FP 0 (from http://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector)
#' @param x vector to check
#' @param tol tolerance
#'
#' @return logical
#' @export
#'
#' @examples x <- c(1, 2, 3, 4, 5, 6, 1)
#' y <- rep(2, times = 7)
#' zero.range(x) # FALSE
#' zero.range(y) # TRUE
#'
zero.range <- function(x, tol = .Machine$double.eps ^ 0.5) {
	if (length(x) == 1) return(TRUE)
	x <- range(x) / mean(x)
	isTRUE(all.equal(x[1], x[2], tolerance = tol))
}
