#' filter_gauss
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
#' \code{filter_gauss} this function applies a gaussian convolution filter to the input signal. the window width is specified by sigma
#' @param signal the input signal
#' @param sigma specifies the window width
#' @export
#'
filter_gauss <- function(signal, sigma) {
	# calculate window size
	alpha <- 2.5
	window_size <- round(sigma * alpha)
	# make sure window size is an odd number
	if (window_size %% 2 == 0) {
		window_size <- window_size + 1
	}
	# generate filter
	gauss_window <- signal::gausswin(window_size, alpha)
	# normalize the filter kernel
	gauss_window <- gauss_window / sum(gauss_window)
	# compute the signal mean
	signal_mean <- mean(signal)
	# convolve zero-mean signal
	signal_conv <- signal::conv(signal - signal_mean, gauss_window)
	# select valid section of the result and add mean
	signal_filtered <- signal_conv(((window_size - 1) / 2 + 1):nrow(signal_filtered) - (window_size - 1) / 2) + signal_mean
	return(signal_filtered)
}
