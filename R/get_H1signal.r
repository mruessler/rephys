get_H1signal <- function(data) {
	#' \code{get_H1signal} a copy of the matlab function with the same name. Since the data are not packed in a container together with metadata it is a simplified version.
	#' @param data data from one recording file (2 channels)
	#' @return the data in a data frame

	return(data.frame(data))
}
