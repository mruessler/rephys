datametamerge <- function() {
	setwd(dir = "~/datarepos/ephysfull/data/")
	dfd <- dir()
	dfd <- data.frame(dfd)
	names(dfd) <- "data"
	dfd <- cbind(dfd, 1:nrow(dfd))
	setwd(dir = "~/datarepos/ephysfull/meta/")
	dfm <- dir()
	dfm <- data.frame(dfm)
	names(dfm) <- "meta"
	dfm <- cbind(dfm, 1:nrow(dfm))
	dfmerge <- merge(dfd, dfm, by.x = "data", by.y = "meta", all = TRUE)
	
	names(dfmerge) <- c("filename", "data", "meta")
	setwd(dir = "~/datarepos/ephysfull/")
	write.csv(x = dfmerge,file = "merge.csv")
}
