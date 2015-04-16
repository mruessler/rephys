prune_spikes <- function(spikes, min_isi) {
	# a copy of the H1toolbox prune_spikes function. input is a spike vector. it is fair to assume that a spike within 3 ms can be filtered out.
	prunedspikes <- spikes
		
	# initialise index of last spike: infinitely before the first one.
 	last <- -Inf
 	for (i in 1:nrow(spikes)) {
		if (n - last < min_isi) {
			spikes_pruned(n,m)=0;  #remove the spike
		}
	}
	
	return(prunedspikes)
}
