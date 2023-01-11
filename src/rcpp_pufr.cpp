#include <Rcpp.h>
using namespace Rcpp;

//' Hamming distance of two vectors
//'
//' @param x A numeric or logical vector
//' @param y A numeric or logical vector
//' @param norm If `TRUE` (default is `FALSE`) normalize the distance to the vector length
//'
//' @return The hamming distance
//'
//' @export
//' @examples
//' hamming_dist(c(0, 1, 0), c(0, 0, 0))
//' hamming_dist(c(0, 1, 0), c(0, 0, 0), norm = TRUE)
// [[Rcpp::export]]
float hamming_dist(NumericVector x, NumericVector y, bool norm = false) {
  float dist = 0;

  if (x.length() != y.length()) {
    throw std::runtime_error("x and y don't have the same length");
  }
  for (int i = 0; i < x.length(); ++i) {
    if (x[i] != y[i]) {
      dist++;
    }
  }

  if (norm == true) {
    return dist / x.length();
  } else {
    return dist;
  }
}

int binom(int n) {
  if (n <= 2) {
    return(n);
  } else {
    return(R::gammafn(n + 1.0) / (2 * R::gammafn((n - 2) + 1.0)));
  }
}

//' Uniqueness of CRPs
//'
//' The uniqueness is calculated as the average of the hamming distance of the CRPs of two devices, for every pair of devices.
//'
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
NumericVector crps_uniqueness(NumericMatrix crps) {
  int n_devices = crps.rows();
  NumericVector interHDs(binom(n_devices));
  int pair_count = 0;
  float hd;

  for(int i = 0; i < n_devices; ++i) {
    for(int j = i + 1; j < n_devices; ++j) {
      hd = 1 - ((float)hamming_dist(crps.row(i), crps.row(j), true));
      interHDs[pair_count++] = hd;
    }
  }
  return interHDs;
}
