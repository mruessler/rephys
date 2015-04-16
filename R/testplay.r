setwd("~/data/ephys/")
setwd("~/wolke/work/ephysdata/")

tsd <- ts(data = d2, start = 0, end = 10, deltat = 1/25000)
plot(tsd)

setwd("~/data/ephys/")
files <- dir()
for (i in 1:length(files)) {
	setwd("~/data/ephys/")
	data <- read.table(files[i], sep = ",", colClasses = "numeric")
	tsd <- ts(data = data, start = 1/25000, end = 10, deltat = 1/25000)
	setwd("~/data/ephys/png/")
	png(filename = sub(pattern = ".csv", replacement = ".png", x = files[i]))
	plot(tsd)
	dev.off()
}
