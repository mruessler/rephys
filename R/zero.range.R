#' zero.range
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
zero.range <- function(x, tol = .Machine$double.eps ^ 0.5) {
	if (length(x) == 1) return(TRUE)
	x <- range(x) / mean(x)
	isTRUE(all.equal(x[1], x[2], tolerance = tol))
}
