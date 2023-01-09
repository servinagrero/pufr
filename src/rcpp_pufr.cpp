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
    throw std::runtime_error("x and y need to have the same length.");
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

//' Uniqueness of a CRP table
//'
//' The uniqueness is calculated as the average of the hamming distance of the CRPs of two devices, for every pair of devices.
//'
//' @param crps A logical or numeric matrix
//'
//' @return The uniqueness of the CRP table
//'
//' @export
//' @examples
//' mat <- matrix(sample(c(0, 1, 100, replace = TRUE)), nrow = 10, ncol = 10)
//' uniqueness(mat)
// [[Rcpp::export]]
float uniqueness(NumericMatrix crps) {
  float uniqueness = 0;
  int n_devices = crps.rows();
  int n_crps = crps.cols();
  int n_pairs = 0;

  for(int i = 0; i < n_devices; ++i) {
    for(int j = i + 1; j < n_devices; ++j) {
      uniqueness += 1 - ((float)hamming_dist(crps.row(i), crps.row(j), true));
      n_pairs++;
    }
  }
  return (uniqueness / n_pairs);
}
