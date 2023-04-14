#include <Rcpp.h>
using namespace Rcpp;


//' Uniqueness of CRPs
//'
//' The uniqueness is calculated as the average of the hamming distance of the CRPs of two devices, for every pair of devices.
//' The number of pairs of devices is calculated as:
//' \deqn{N = \binom{D}{2} = \frac{D!}{2 \cdot (D - 2)!}}
//' where D represents the number of devices.
//'
//' @param crps A logical or numeric matrix
//'
//' @return The uniqueness of the CRP table
//'
//' @export
//' @examples
//' mat <- matrix(sample(c(0, 1, 100, replace = TRUE)), nrow = 10, ncol = 10)
//' crps_uniqueness(mat)
// [[Rcpp::export]]
NumericVector crps_uniqueness(const NumericMatrix &crps) {
  int n_devices = crps.rows();
  int npairs = R::gammafn(n_devices + 1.0) / (2 * R::gammafn((n_devices - 2) + 1.0));
  NumericVector interHDs(npairs);
  int pair_count = 0;

  Environment pufr("package:pufr");
  Function hamming_dist = pufr["hamming_dist"];

  for (int i = 0; i < n_devices; ++i) {
    for (int j = i + 1; j < n_devices; ++j) {
      NumericVector hd = hamming_dist(_["x"] = crps.row(i),
                                      _["y"] = crps.row(j), _["norm"] = true);
      interHDs[pair_count++] = 1 - hd[0];
    }
  }
  return interHDs;
}
