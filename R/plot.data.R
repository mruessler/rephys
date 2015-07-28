#' plot.data
#'
#' @param spikes data frame of spike data
#' @param filelist
#' @param stimlist
#' @param outputdir
#'
#' @return nothing yet
#' @export
#'
plot.data <- function(spikes, filelist, stimlist, outputdir) {
	spikes <- data.frame(spikes)
	# plotting loop
	i <- 1
	while (i < length(filelist)) {
		png(width = 1500, height = 1000, filename = sub(pattern = ".csv", replacement = "--spikes.png", x = paste0(outputdir, filelist)[i]))
		grid::grid.newpage()
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
		vplayout <- function(x, y) {
			grid::viewport(layout.pos.row = x, layout.pos.col = y)
		}
		# cut a slice of data for ggplot consumption
		curr.spikes <- data.frame(time, spikes[, i:(i + 1)])
		# name the columns for easy attribution
		colnames(curr.spikes) <- c("time", "left", "right")
		ggspikes <- ggplot2::ggplot(data = spikes[, i], ggplot2::aes(x = 1:nrow(spikes), y = left)) +
			ggplot2::geom_point(stat = "identity")
 		print(ggspikes, vp = vplayout(1, 1:2))
# 		print(b, vp = vplayout(2, 1))
# 		print(c, vp = vplayout(2, 2))
		dev.off()

	}
}
# ggplot(data = head(topcountries, 10), aes(x = reorder(Country, Earnings), y = Earnings / 1000000)) +
# 	geom_bar(stat = "identity") +
# 	geom_text(aes(label = round(Earnings / 1000000, digits = 1), y = 0.3), colour = "white") +
# 	coord_flip() +
# 	theme_minimal(base_size = 20, base_family = "Raleway") +
# 	ggtitle("Top earnings per country (2014)") +
# 	xlab("Country") +
# 	ylab(expression("Earnings [" ~ 10 ^ {6} ~ "US$]"))

# plot.data <- function(spikes, stimlist, files, pd) {
# 	# plot the spikes and the stimuli
# 	start.time <- timer()
# 	for (i in 1:length(files)) {
# 		png(width = 1500, filename = sub(pattern = ".csv", replacement = "--spikes.png", x = paste0(pd, files)[i]))
# 		par(mfrow = c(4, 1), bty = "n", cex = 1)
# 		tsspikes <- ts(data = spikes[((i * 2) - 1):(i * 2)], start = 1/25000, end = 10, deltat = 1/25000)
# 		print(head(tsspikes))
# 		plot.spikes(tsspikes[, 1], xlab = "", ylab = "Left channel", xaxt = "n", main = files[i])
# 		plot.spikes(tsspikes[, 2], xlab = "", ylab = "Right channel")
# 		plot.stimulus(stimlist[[i]])
# 		dev.off()
# 	}
# 	time.diff <- timer(start.time)
# 	writeLines(paste0("Plotting completed (", time.diff, " seconds)."))
# }
