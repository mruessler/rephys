//' @name prunespikes
//'
//' @title prunespikes
//' @description A C++ implementation of a demanding function
//' @param spikes spike matrix to be pruned
//' @param minisi minimum inter stimulus interval in milliseconds
//' @return a matrix containing a pruned spike
//' @export

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
