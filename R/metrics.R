## usethis namespace: start
#' @useDynLib pufr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
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
#' @return The entropy of the vector
#'
#' @export
entropy_shannon <- function(v) {
  freqs <- table(v) / length(v)
  -sum(freqs * log2(freqs))
}

#' Shannon entropy for binary vectors
#'
#' The probability \eqn{p(1)} corresponds to the normalized [hamming_weight] of the vector.
#'
#' @param v A binary vector
#'
#' @return The entropy of the vector
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
#' @param v A vector of probabilities
#'
#' @return The Shannon entropy of each probability
#'
#' @seealso [entropy_shannon][pufr::entropy_shannon]
#' @export
entropy_p <- function(v) {
  p_zero <- 1 - v
  vals <- -((v * log2(v)) + (p_zero * log2(p_zero)))
  ifelse(is.nan(vals), 0, vals)
}


#' Hamming weight of CRPs
#'
#' @description
#' This function is a wrapper around [hamming_weight][pufr::hamming_weight] to be used on vectors and 2D matrix.
#'
#' This function assumes that if the CRPs are supplied in a 2D matrix, each row corresponds to a device and each column corresponds to a CRP. If another convetion is used, modify the parameter `margin` accordingly.
#'
#' @details
#' ### Uniformity
#'
#' Uniformity measures the distribution of 1s and 0s across all CRPs for each device. To calculate uniformity, `margin` should be 1.
#'
#' \deqn{Uniformity = \frac{1}{\#C} \sum_{c = 0}^{\#C} crp_c}
#'
#' ### Bitaliasing
#'
#' Bitaliasing measures the distribution of 1s and 0s for a single CRPs across all devices. To calculate bitaliasing, `margin` should be 2.
#'
#' \deqn{Bitaliasing = \frac{1}{\#D} \sum_{d = 0}^{\#D} crp_d}
#'
#' @param crps A binary vector or 2D matrix.
#' @param margin The margin to calculate the Hamming weight. 1 for rows and 2 for columns.
#'
#' @return The Hamming weight of the CRPs
#' @export
#'
#' @seealso [hamming_weight][pufr::hamming_weight]
#' @examples
#' ## Hamming weight of a binary vector
#' v <- rbits(50)
#' crps_weight(v)
#'
#' ## Uniformity of a set of CRPs
#' mat <- matrix(rbits(500), nrow = 5, ncol = 10)
#' crps_weight(mat, 1)
#'
#' ## Bitaliasing of a set of CRPs
#' crps_weight(mat, 2)
crps_weight <- function(crps, margin = 1) {
  if (is.vector(crps)) {
    return(hamming_weight(crps, norm = TRUE))
  } else if (is.matrix(crps)) {
    return(par_apply(crps, margin, hamming_weight, norm = TRUE))
  }
  cli::cli_abort("crps needs to be a vector or a 2D matrix, not {.type {crps}}")
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
#' @param ref_sample Numeric index for the reference sample: If `crps` is a vector, is the index of the reference sample; If `crps` is a 2D matrix, the row to use as reference; If `crps` is a 3D array,the row for all 3rd dimension matrix.
#'
#' @return If `crps` is a vector, the intra Hamming distance of the vector. If `crps` is a 2D matrix, the reliability of each column as a vector of size \code{ncol(crps) - 1}. If `crps` is a 3D array, a 2D matrix where each row contains the intra Hamming distance all samples.
#'
#' @export
#' @seealso [hamming_dist][pufr::hamming_dist]
#'
#' @examples
#' #' ## Bit values
#' v <- c(1, 0, 0, 1, 1)
#' intra_hd(v, 1)
#'
#' ## Set of CRPs
#' mat <- matrix(rbits(50), nrow = 5, ncol = 10)
#' intra_hd(mat, 1)
#'
#' ## Set of devices with their respective samples
#' mat <- array(rbits(150), dim = c(5, 10, 3))
#' intra_hd(mat)
intra_hd <- function(crps, ref_sample = 1) {
  if (is.vector(crps)) {
    if (length(crps) == 1) {
      return(1)
    }
    if (ref_sample < 1 || ref_sample > length(crps)) {
      cli::cli_abort("ref_sample should be in the range [1, {length(crps)}]")
    }
    sample_ids <- setdiff(seq_along(crps), ref_sample)
    helper <- function(i) {
      hamming_dist(crps[ref_sample], crps[i], norm = TRUE)
    }
    return(mean(par_vapply(sample_ids, helper, numeric(1))))
  } else if (is.matrix(crps)) {
    return(par_apply(crps, 2, intra_hd, ref_sample))
  } else if (is.array(crps)) {
    return(t(par_apply(crps, 1, function(s) intra_hd(t(s), ref_sample))))
  }
  cli::cli_abort("crps should be a 2D matrix or 3D array, not {.type {crps}}")
}

#' Reliability of CRPs
#'
#' The reliability is defined as \eqn{1 - Intra_{HD}}
#' @inheritParams intra_hd
#' @export
#' @seealso [intra_hd][pufr::intra_hd]
#'
#' @examples
#' ## Set of CRPs
#' mat <- matrix(rbits(200), nrow = 10, ncol = 20)
#' intra <- intra_hd(mat, 1)
#' all(1 - intra == reliability(mat, 1))
reliability <- function(crps, ref_sample = 1) {
  1 - intra_hd(crps, ref_sample)
}

#' @rdname uniqueness
#' @export
inter_hd <- uniqueness
