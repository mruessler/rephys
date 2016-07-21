#' plot_quickmatrix
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
#' \code{startanalysis} This function plots each column of a data matrix
#' @param matrix is the data matrix
#' @param filename is the filename prefix
#' @export
plot_quickmatrix <- function(matrix, filename = "spike") {
	for (i in 1:ncol(matrix)) {
		png(filename = paste(filename, i, ".png", sep = ""))
		if (any(!is.na(matrix[, i]))) {
			plot(matrix[, i], type = "l")
		}
		dev.off()
	}
	print("fin")
}
