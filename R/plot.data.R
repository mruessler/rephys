#' plot.data
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
#' @param spikes data frame of spike data
#' @param filelist list of files to plot
#' @param stimlist currently not used
#' @param outputdir the output directory where the plots will be saved
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
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(3, 1)))
		# cut a slice of data for ggplot consumption
		time <- 1:nrow(spikes)
		curr.spikes <- data.frame(time, spikes[, i:(i + 1)])
		# name the columns for easy attribution
		colnames(curr.spikes) <- c("time", "left", "right")
		# remove unnecessary zero values
		curr.spikes[curr.spikes == 0] <- NA
		curr.spikes$right <- curr.spikes$right + 1
		ggspikes <- ggplot2::ggplot(data = curr.spikes, ggplot2::aes(x = time, y = left)) +
			ggplot2::geom_point(stat = "identity") +
			ggplot2::geom_point(stat = "identity", ggplot2::aes(y = right)) +
			ggplot2::xlim(0, 3)

 		print(ggspikes, vp = vplayout(1, 1))
 		print(ggspikes, vp = vplayout(2, 1))
# 		print(c, vp = vplayout(2, 2))
		dev.off()
		i <- i + 2
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
