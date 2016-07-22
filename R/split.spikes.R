#' split.spikes
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
#' @description In the beginning there was the spike matrix. We want to divide it into two in order to have the channels separated.
#' @param spikes spike data
#' @return nothing
#' @export
#'
split.spikes <- function(spikes) {
	spikes.left <- spikes[, seq(1, ncol(spikes) - 1, 2)]
	spikes.right <- spikes[, seq(2, ncol(spikes), 2)]

	psth.left <- get.psth(spikes.left, binwidth = 0)
	isipsth.left <- get.isipsth(spikes.left)

	psth.right <- get.psth(spikes.right)
	isipsth.right <- get.isipsth(spikes.right)
}
