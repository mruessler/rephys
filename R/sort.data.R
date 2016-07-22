#' sort.data
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
#' @description menu-like programs to view data
#'
#' @return nothing
#' @export
#'
sort.data <- function(folder = NULL) {
	# if folder is not set do it interactively
	if (is.null(folder)) {
		folder <- sort.data.get.folder()
	}
	# we are just the face–fast-forward to the next function
	sort.data.main(folder)
}

sort.data.main <- function(folder, files = NULL, rawdata = NULL, trial = 1) {
	# read files if not already done
	if (is.null(files)) {
		files <- dir(folder, pattern = ".csv")
		writeLines(paste0("Reading ", length(files), " files..."))
		rawdata <- read.ephysdata(files, folder = folder)
	}
	# plot current trial
	sort.data.plot.trial(trial = rawdata[, ((trial * 2) - 1):(trial * 2)], filename = files[trial])
	# interactive part: ask the user what to do
	choice <- menu(c("Previous trial",
									 "Next trial",
									 "Plot threshold",
									 "Set threshold",
									 "Quit"),
								 title = "What do you want to do?")
	while (choice != 5) {
		# show next trial
		if (choice == 2) {
			if (trial < length(files)) {
				sort.data.main(folder, files, rawdata, trial = trial + 1)
			}
			else {
				writeLines("You are already looking at the last trial!")
				sort.data.main(folder, files, rawdata, trial = length(files))
			}
		}
		# show previous trial
		if (choice == 1) {
			if (trial > 1) {
				sort.data.main(folder, files, rawdata, trial = trial - 1)
			}
			else {
				writeLines("You are already looking at the first trial!")
				sort.data.main(folder, files, rawdata, trial = 1)
			}
		}
		# plot a threshold
		if (choice == 3) {
			choice <- as.numeric(readline(prompt = "For which channel (1/2)? "))
			sort.data.plot.threshold(rawdata, trial, channel = choice)
		}
		if (choice == 5) {
			break
		}
	}
	# exit
	writeLines("Closing...!")
}

sort.data.get.folder <- function() {
	folder <- readline(prompt = "Enter a folder (.../folder/data)! ")
	# check whether the folder exists
	while (!file.exists(folder)) {
		folder <- readline(prompt = "Could not read from folder. Enter a valid folder address! ")
	}
	return(folder)
}

sort.data.plot.trial <- function(trial, filename) {
	# plot current trial
	par(oma = c(rep(0, 4)), mar = c(1, 1, 1, 1), ps = 20, cex = 10)
	layout(matrix(1:2, 2, 1), heights = c(0.5, 0.5))
	# Todo: plot one channel at a time
	plot.trial(trial = trial, filename = filename)
}

sort.data.plot.threshold <- function(rawdata, trial, channel = 0, threshold = NULL) {
	# plot the threshold
	# compute threshold value; m and s have the dim(1, 2)
	if (is.null(threshold)) {
		m <- mean(rawdata[, trial * 2 - 2 + channel])
		s <- sd(rawdata[, trial * 2 - 2 + channel])
		threshold <- m + s
	}
	abline(h = threshold, col = "red")
}

sort.data.select.channel <- function() {

}

sort.data.write.threshold <- function() {

}

sort.data.delete.file <- function() {

}

sort.data.change.file <- function() {

}

