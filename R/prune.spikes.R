#' prune.spikes
#'
#' @description a copy of the H1toolbox prune_spikes function. This Function removes spikes that fall within the minimum inter stimulus interval of the previous spike. It is safe to assume that a spike within 3 ms can be filtered out.
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
 					prunedspikes[j, i] <- 0;  # remove the spike
 				}
 				else {
 					last <- j
 				}
 			}
 		}
	}
	return(prunedspikes)
}

prune.cols <- function(spikes, min.isi) {
	prunedspikes <- apply(spikes, 2, FUN = prune.rows, min.isi = min.isi)
	return(prunedspikes)
}

prune.rows <- function(spikes, min.isi) {
	prunedspikes <- spikes
	last <- -Inf
	for (i in 1:length(spikes)) {
		if (spikes[i] == 1) {
			if (i - last < min.isi) {
				prunedspikes[i] <- 0;  # remove the spike
			}
			else {
				last <- i
			}
		}
	}
	return(prunedspikes)
}

# try from SE
# prune.rows <- function(spikes, min.isi) {       
# 	one_idx_vec <- which(spikes == 1)
# 	if (length(one_idx_vec) < 2) {
# 		return(spikes)
# 	}
# 	dist <- one_idx_vec[-1] - head(one_idx_vec, -1)
# 	less_than_min <- (dist < min.isi)
# 	spikes[one_idx_vec[c(F,less_than_min)]] <- 0
# 	
# 	return(spikes)
# }
