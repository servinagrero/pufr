## usethis namespace: start
#' @useDynLib pufr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom cowplot plot_grid
#' @import ggplot2
#' @import dplyr
#' @import viridis
#' @import cli
#' @importFrom stats rbinom
#' @importFrom reshape2 melt
## usethis namespace: end
NULL

#' Shannon entropy for numeric values
#'
#' @description
#' The Shannon entropy of a vector is calculated as:
#' \deqn{H(X) := -\sum_{x \in \chi} p(x) \log p(x) = E[-\log p(x)]}
#'
#' @details
#' When calculating the probabilities, it is assumed by convention that \eqn{(0\cdot \log 0 = 0)} and \eqn{(1 \cdot \log 1 = 1)}.
#'
#' @param v A vector of values
#'
#' @returns The Shannon entropy of the vector
#'
#' @export
#' @examples
#' v <- sample(1:10, 50, TRUE, rep(0.1, 10))
#' entropy_shannon(v)
entropy_shannon <- function(v) {
  freqs <- table(v) / length(v)
  -sum(freqs * log2(freqs))
}

#' Shannon entropy for binary vectors
#'
#' @details
#' The probability \eqn{p(1)} corresponds to the normalized [hamming_weight] of the vector.
#'
#' @param v A binary vector
#'
#' @returns The Shannon entropy of the vector
#'
#' @export
#' @seealso [entropy_shannon][pufr::entropy_shannon]
#' @examples
#' entropy_bits(c(0, 0, 0))
#' entropy_bits(c(1, 1, 1))
#' entropy_bits(rbits(20))
entropy_bits <- function(v) {
  p_ones <- hamming_weight(v, norm = TRUE)
  entropy_p(p_ones)
}

#' Shannon entropy for probabilities
#'
#' @param v A vector of probabilities where each probability is treated as P(1)
#'
#' @returns The Shannon entropy of each probability
#'
#' @seealso [entropy_shannon][pufr::entropy_shannon]
#' @export
entropy_p <- function(v) {
  p_zero <- 1 - v
  vals <- -((v * log2(v)) + (p_zero * log2(p_zero)))
  ifelse(is.nan(vals), 0, vals)
}

#' Uniformity of a bit vector or a CRP matrix
#'
#' This function assumes that if the CRPs are supplied in a 2D matrix, each row corresponds to a device and each column corresponds to a CRP.
#'
#' @details
#' Uniformity measures the distribution of 1s and 0s across all CRPs for each device. To calculate uniformity, `margin` should be 1.
#'
#' \deqn{Uniformity = \frac{1}{\#C} \sum_{c = 0}^{\#C} crp_c}
#'
#' @param crps A bit vector or a 2D CRP matrix
#'
#' @returns If `crps` is a vector, the normalized Hamming weight. If `crps` is a matrix, the normalized Hamming weight of each row.
#'
#' @export
#' @seealso [hamming_weight][pufr::hamming_weight]
#' @examples
#' ## Uniformity of a vector
#' uniformity(rbits(50))
#'
#' ## Uniformity of a matrix
#' uniformity(rbits(c(10, 50)))
uniformity <- function(crps) {
  stopifnot(is.vector(crps) || is.matrix(crps))
  if (is.vector(crps)) {
    hamming_weight(crps, norm = TRUE)
  } else if (is.matrix(crps)) {
    apply(crps, 1, hamming_weight, norm = TRUE)
  }
}

#' Bitaliasing of a CRP matrix
#'
#' Unlike `Uniformity`, it does not make sense to calculate the bitaliasing of a single bit vector. Thus, `bitaliasing` only works on 2D matrix.
#'
#' Like `Unformity`, this function assumes that the CRPs are supplied in a 2D matrix, where each row corresponds to a device and each column corresponds to a CRP.
#'
#' @details
#' Bitaliasing measures the distribution of 1s and 0s for a single CRPs across all devices.
#'
#' \deqn{Bitaliasing = \frac{1}{\#D} \sum_{d = 0}^{\#D} crp_d}
#'
#' @param crps A bit vector or a 2D CRP matrix
#'
#' @returns The normalized Hamming weight of each column.
#'
#' @export
#' @seealso [hamming_weight][pufr::hamming_weight]
#' @examples
#' ## Bitaliasing of a matrix
#' bitaliasing(rbits(c(5, 50)))
bitaliasing <- function(crps) {
  stopifnot(is.matrix(crps))
  apply(crps, 2, hamming_weight, norm = TRUE)
}

#' Intra Hamming distance of CRPs
#'
#' @description
#' The intra Hamming distance of two sets of CRPs measures the numbers of CRPs that differ. N sets of CRPs are equal if their intra Hamming distances are 0.
#'
#' A response is reliable if it does not change in time, thus, its intra Hamming distance is equal to 0.
#'
#' If `crps` is a 2D matrix, it is assumed that each row corresponds to a sample and each column to a CRP. The `ref_sample` will say which row is the sample taken as reference. In the case of a 3D matrix, each row corresponds to a device, each column to a CRP and each 3rd dimension to a different sample.
#'
#' @details
#' The order of the samples is calculated as \code{setdiff(seq_len(nsamples), ref_sample)} where `nsamples` corresponds to \code{nrow(crps)} in the case of a 2D matrix and \code{dim(crps)[3]} in the case of a 3D matrix.
#'
#' @param crps A binary vector, 2D matrix or 3D array.
#' @param ref Numeric index for the reference sample: If `crps` is a vector, is the index of the reference sample; If `crps` is a 2D matrix, the row to use as reference; If `crps` is a 3D array,the row for all 3rd dimension matrix.
#'
#' @returns If `crps` is a vector, the intra Hamming distance of the vector. If `crps` is a 2D matrix, the reliability of each column as a vector of size \code{ncol(crps) - 1}. If `crps` is a 3D array, a 2D matrix where each row contains the intra Hamming distance all samples.
#'
#' TODO: Maybe return the list of comparison to calculate the mean and sd
#'
#' @export
#' @seealso [hamming_dist][pufr::hamming_dist]
#'
#' @examples
#' ## Bit values
#' v <- c(1, 0, 0, 1, 1)
#' intra_hd(v, 1)
#'
#' ## Set of CRPs
#' mat <- rbits(c(5, 10))
#' intra_hd(mat, 1)
#'
#' ## Set of devices with their respective samples
#' mat <- rbits(c(5, 10, 3))
#' intra_hd(mat)
intra_hd <- function(crps, ref = 1) {
  compare_with_ref <- function(x, ref = 1) {
    if (!(ref >= 1 & ref <= length(x))) {
      cli::cli_abort("ref_sample should be in the range [1, {length(x)}]")
    }
    1 - bitwXor(x[ref], x[setdiff(seq_along(x), ref)])
  }

  if (is.vector(crps)) {
    return(compare_with_ref(crps, ref))
  }

  if (is.matrix(crps)) {
    return(apply(crps, 2, function(x) compare_with_ref(x, ref)))
  }

  if (is.array(crps)) {
    return(apply(crps, c(1, 2), function(x) mean(compare_with_ref(x, ref))))
  }

  return(1)
}

#' Reliability of CRPs
#'
#' The reliability of a PUF is defined as \eqn{1 - Intra_{HD}}
#' @inheritParams intra_hd
#' @export
#' @seealso [intra_hd][pufr::intra_hd]
#'
#' @examples
#' ## Set of CRPs
#' mat <- matrix(rbits(200), nrow = 10, ncol = 20)
#' intra <- intra_hd(mat, 1)
#' all(1 - intra == reliability(mat, 1))
reliability <- function(crps, ref = 1) {
  1 - intra_hd(crps, ref)
}

#' @rdname uniqueness
#' @export
inter_hd <- uniqueness
