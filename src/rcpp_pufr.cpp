#include <Rcpp.h>
using namespace Rcpp;

#include <omp.h>
// [[Rcpp::plugins(openmp)]]

//' Uniqueness of CRPs
//'
//' The uniqueness is calculated as the average of the hamming distance of the CRPs of two devices, for every pair of devices.
//' The number of pairs of devices is calculated with the following formula, where \eqn{D} is the number of devices.
//' \deqn{N = \frac{D(D-1)}{2}}
//'
//' @param crps A logical or numeric matrix
//'
//' @return The uniqueness of the CRP table
//'
//' @export
//' @examples
//' mat <- rbits(c(10, 10))
//' uniqueness(mat)
// [[Rcpp::export]]
NumericVector uniqueness(const NumericMatrix &crps) {
  int n_devices = crps.rows();
  size_t npairs = (n_devices * (n_devices - 1)) / 2;

  if (npairs >= SIZE_MAX) {
    throw std::invalid_argument("Number of pairs should be smaller than 2^32 - 1");
  }

  NumericVector interHDs(npairs);
  int pair_count = 0;

  Environment pufr("package:pufr");
  Function hamming_dist = pufr["hamming_dist"];

  // FIXME: For some reason parallel crashes the whole function
  // #pragma omp parallel for shared(crps,pair_count) private(i,j)
  #pragma omp for
  for (int i = 0; i < n_devices - 1; ++i) {
    for (int j = i + 1; j < n_devices; ++j) {
      NumericVector hd = hamming_dist(_["x"] = crps.row(i),
                                      _["y"] = crps.row(j), _["norm"] = true);
      interHDs[pair_count++] = hd[0];
    }
  }
  return interHDs;
}
