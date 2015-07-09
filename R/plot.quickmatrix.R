#' plot.quickmatrix
#' 
#' \code{plot.quickmatrix} This function plots each column of a data matrix
#' @param matrix is the data matrix
#' @param filename is the filename prefix
#' @export
plot.quickmatrix <- function(matrix, filename = "spike") {
	for (i in 1:ncol(matrix)) {
		png(filename = paste0(filename, i, ".png"))
		if (any(!is.na(matrix[, i]))) {
			plot(matrix[, i], type = "l")
		}
		dev.off()
	}
	print("fin")
}
