#' prune.spikes
#'
#' @description a copy of the H1toolbox prune_spikes function. This Function removes spikes that fall within the minimum inter stimulus interval of the previous spike. It is safe to assume that a spike within 3 ms can be filtered out.
#' @param spikes spike matrix to be pruned
#' @param min.isi minimum inter stimulus interval in milliseconds
#'
#' @return a matrix containing a pruned spike
#' @export
#'
prune.spikes <- function(spikes, min.isi) {
	# copy spike matrix
	prunedspikes <- spikes
	
	# initialise index of last spike: infinitely before the first one.
 	for (i in 1:ncol(spikes)) {
 		last <- -Inf
 		for (j in 1:nrow(spikes)) {
 			if (spikes[j, i] == 1) {
 				if (j - last < min.isi) {
 					prunedspikes[j, i] <- 0;  #remove the spike
 				}
 				else {
 					last <- j
 				}
 			}
 		}
		
	}
	return(prunedspikes)
}
