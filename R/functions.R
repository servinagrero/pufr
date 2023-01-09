## usethis namespace: start
#' @useDynLib pufr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL


#' Hamming weight of a vector
#'
#' The hamming weight is the number of non null symbols in a vector. In this case, the number of 1s.
#'
#' @param v A logical or numeric vector
#'
#' @return The hamming weight
#'
#' @export
#' @examples
#' hamming_weight(c(0, 1, 1))
#' hamming_weight(c(TRUE, TRUE, TRUE))
hamming_weight <- function(v) {
  sum(as.logical(v), na.rm = TRUE)
}

#' Optimized Shannon entropy for logical vectors
#'
#' @param v A logical or numeric vector
#'
#' @return The Shannon entropy of the vector
#'
#' @export
#' @examples
#' entropy_lgl(c(0, 0, 0))
#' entropy_lgl(sample(c(0, 1), 20, replace = TRUE))
entropy_lgl <- function(v) {
  p_ones <- sum(v, na.rm = TRUE) / length(v)
  p_zeros <- 1 - p_ones
  if (p_ones == 0) {
    - (p_zeros * log2(p_zeros))
  } else if (p_zeros == 0) {
    - (p_ones * log2(p_ones))
  } else {
    - ((p_ones * log2(p_ones)) + (p_zeros * log2(p_zeros)))
  }

}

#' Ratio of bits in a vector
#'
#' The ratio is calculated as the number of ones minus the number of zeros.
#'
#' \deqn{Ratio = \frac{\sum_{i=0}^{n} v_i}{n}}
#'
#' If the ratio is not absolute, a positive result indicates that there are more 1s than 0s, while a negative results indicates the opposite.
#'
#' @param v A logical or numeric vector
#' @param abs If `TRUE` (default) the ratio is absolute
#'
#' @return The ratio of bits
#'
#' @export
#' @examples
#' ratio_bits(c(TRUE, FALSE, TRUE), abs = TRUE)
#' ratio_bits(c(0, 1, 0), abs = FALSE)
#' ratio_bits(c(0, 1, 0, 1))
ratio_bits <- function(v, abs = TRUE) {
  n_ones <- sum(v)
  if (abs == TRUE) {
    abs((2 * n_ones) - length(v)) / length(v)
  } else {
    ((2 * n_ones) - length(v)) / length(v)
  }
}
