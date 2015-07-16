plot_quickmatrix <- function(matrix, filename = "spike") {
	#' \code{startanalysis} This function plots each column of a data matrix
	#' @param matrix is the data matrix
	#' @param filename is the filename prefix
	for (i in 1:ncol(matrix)) {
		png(filename = paste(filename, i, ".png", sep = ""))
		if (any(!is.na(matrix[, i]))) {
			plot(matrix[, i], type = "l")
		}
		dev.off()
	}
	print("fin")
}
