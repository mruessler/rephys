# in the beginning there was the spike matrix
# we want to divide it into two in order to have the channels separated
split.spikes <- function(spikes) {
	spikes.left <- spikes[, seq(1, ncol(spikes) - 1, 2)]
	spikes.right <- spikes[, seq(2, ncol(spikes), 2)]

	psth.left <- get.psth(spikes.left, binwidth = 0)
	isipsth.left <- get.isipsth(spikes.left)

	psth.right <- get.psth(spikes.right)
	isipsth.right <- get.isipsth(spikes.right)
}
