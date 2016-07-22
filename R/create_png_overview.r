#' create_png_overview
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
#' \code{create_png_overview} A function to create png overviews of ephys log files
#' @param wd working directory where the data is. It is required to use the folder containing the data, meta, and png subfolders as input
#' @export
#'
create_png_overview <- function(wd = NA) {
	# check whether a working directory was specified
	if (is.na(wd) == TRUE) {
		stop("Error, no directory specified.")
	}
	else {
		dd <- paste(wd, "/data", sep = "")
	}
	# get the files from folder
	files <- dir(dd, pattern = ".csv")
	# get the data from the files
	data <- readephysdata(files, folder = dd, mc = TRUE)
	# plot the data from the files
	print("Start writing png files")
	for (i in 1:length(files)) {
		tsdata <- ts(data = data[i], start = 1/25000, end = 10, deltat = 1/25000)
		png(filename = sub(pattern = ".csv", replacement = ".png", x = paste(wd, "/png/", files, sep = "")[i]))
		plot(tsdata, xlab = "Time (s)", ylab = "Amplitude", main = files[1])
		dev.off()
	}
}
