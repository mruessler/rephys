//' This file is part of the rephys package.
//'
//' Copyright(c) Martin Rüßler
//'
//' This file may be licensed under the terms of of the
//' GNU General Public License Version 3 (the ``GPL''),
//' or (at your option) any later version.
//'
//' Software distributed under the License is distributed
//' on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY KIND, either
//' express or implied. See the GPL for the specific language
//' governing rights and limitations.
//'
//' You should have received a copy of the GPL along with this
//' program. If not, go to http://www.gnu.org/licenses/gpl.html
//' or write to the Free Software Foundation, Inc.,
//' 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
//'
//' The development of this software was supported by the
//' Excellence Cluster EXC 277 Cognitive Interaction Technology.
//' The Excellence Cluster EXC 277 is a grant of the Deutsche
//' Forschungsgemeinschaft (DFG) in the context of the German
//' Excellence Initiative.
//'
//' @name prunespikes
//' @title prunespikes
//' @description A C++ implementation of a resource-heavy function
//' @param spikes spike matrix to be pruned
//' @param minisi minimum inter stimulus interval in milliseconds
//' @return a matrix containing a pruned spike
//' @export
//'
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix prunespikes(NumericMatrix spikes, double minisi) {
	NumericMatrix prunedspikes = spikes;
	int ncol = spikes.ncol();
	int nrow = spikes.nrow();
	for (int i = 0; i < ncol; i++) {
		int last = 0;
		while (spikes(last, i) == 0) {
			last++;
		}
		for (int j = last + 1; j < nrow; j++) {
			if (spikes(j, i) == 1) {
				if (j - last < minisi) {
					prunedspikes(j, i) = 0;
				} else {
					last = j;
				}
			}
		}
	}
  return prunedspikes;
}
