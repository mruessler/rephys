#' vplayout
#'
#' @description a small convenience function
#' @param x x coordinate of the viewport
#' @param y y coordinate of the viewport
#'
#' @return viewport layout
vplayout <- function(x, y) {
	grid::viewport(layout.pos.row = x, layout.pos.col = y)
}
