#' prune.spikes
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
