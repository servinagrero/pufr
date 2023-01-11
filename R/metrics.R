## usethis namespace: start
#' @useDynLib pufr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

#' Shannon entropy optimized for binary vectors
#'
#' @description
#' The Shannon entropy of a vector is calculated as:
#' \deqn{H(X) := -\sum_{x \in \chi} p(x) \log p(x) = E[-\log p(x)]}
#' where \eqn{p(x)} refers to \eqn{p(0)} and \eqn{p(1)} in a binary vector.
#' By convention, it is assumed that \eqn{0 \log 0 = 0} and \eqn{1 \log 1 = 1}.
#'
#' The probability \eqn{p(1)} corresponds to the normalized [hamming_weight] of the vector.
#'
#' @param v A binary vector
#'
#' @return The Shannon entropy of the vector
#'
#' @export
#' @seealso [hamming_weight][pufr::hamming_weight]
#' @examples
#' entropy_bits(c(0, 0, 0))
#' entropy_bits(rbits(20))
entropy_bits <- function(v) {
  p_ones <- hamming_weight(v, norm = TRUE)
  p_zeros <- 1 - p_ones
  if (p_ones == 0) {
    -(p_zeros * log2(p_zeros))
  } else if (p_zeros == 0) {
    -(p_ones * log2(p_ones))
  } else {
    -((p_ones * log2(p_ones)) + (p_zeros * log2(p_zeros)))
  }
}

#' Hamming weight of CRPs
#'
#' @description
#' This function is a wrapper around [hamming_weight][pufr::hamming_weight] to be used on vectors and 2D matrix.
#'
#' This function assumes that if the CRPs are supplied in a 2D matrix, each row corresponds to a device and each column corresponds to a CRP. If another convetion is used, modify the parameter `axis` accordingly.
#'
#' @details
#' ### Uniformity
#'
#' Uniformity measures the distribution of 1s and 0s across all CRPs for each device. To calculate uniformity `axis` should be 1.
#'
#' \deqn{Uniformity = \frac{1}{\#C} \sum_{c = 0}^{\#C} crp_c}
#'
#' ### Bitaliasing
#'
#' Bitaliasing measures the distribution of 1s and 0s for a single CRPs across all devices. To calculate bitaliasing `axis` should be 2.
#'
#' \deqn{Bitaliasing = \frac{1}{\#D} \sum_{d = 0}^{\#D} crp_d}
#'
#' @param crps A binary vector or 2D matrix.
#' @param axis The axis to calculate the Hamming weight.
#'  1 for rows and 2 for columns.
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
crps_weight <- function(crps, axis = 1) {
  if (is.vector(crps)) {
    return(hamming_weight(crps, norm = TRUE))
  } else if (is.matrix(crps)) {
    if (!(axis %in% c(1, 2))) {
      cli::cli_abort("axis can only be 1 or 2, not {axis}")
    }
    if (!is.null(pufr_env$ctx)) {
      return(
        parallel::parApply(
          pufr_env$ctx, crps, axis, hamming_weight,
          norm = TRUE
        )
      )
    } else {
      return(apply(crps, axis, hamming_weight, norm = TRUE))
    }
  }
  cli::cli_abort("crps needs to be a vector or a 2D matrix")
}


#' Intra Hamming distance of CRPs
#'
#' @description
#' The intra Hamming distance of two sets of CRPs measures the numbers of CRPs that differ. N sets of CRPs are equal if their intra Hamming distances are 0.
#'
#' A response is reliable if it does not change in time, thus, its intra Hamming distance is equal to 0.
#'
#' @param crps A binary 2D matrix or 3D array
#' @param ref_sample Row of the 2D matrix used as reference CRPs
#'
#' @return The average intra Hamming distance
#' @export
#' @seealso [hamming_dist][pufr::hamming_dist]
#'
#' @examples
#' ## Set of CRPs
#' mat <- matrix(0, nrow = 10, ncol = 50)
#' intra_hd(mat, 1)
#'
#' ## Set of devices with their respective samples
#' mat <- array(rbits(10 * 50 * 3), dim = c(10, 50, 3))
#' colMeans(intra_hd(mat))
intra_hd <- function(crps, ref_sample = 1) {
  if (is.matrix(crps)) {
    ncrps <- nrow(crps)
    if (!(ref_sample %in% seq_len(ncrps))) {
      cli::cli_abort("ref_sample should be in the range [1, {nrow(crps)}]")
    }
    sample_ids <- setdiff(seq_len(ncrps), ref_sample)
    intra_hd_fn <- function(i) {
      hamming_dist(crps[ref_sample, ], crps[i, ], norm = TRUE)
    }

    if (!is.null(pufr_env$ctx)) {
      dists <- parallel::parLapply(
        pufr_env$ctx, sample_ids, intra_hd_fn,
        norm = TRUE
      )
    } else {
      dists <- vapply(sample_ids, intra_hd_fn, numeric(1))
    }
    return(dists)
  } else if (is.array(crps)) {
    return(rowSums(crps, dims = 2))
  }
  cli::cli_abort("crps should be a 2D matrix or 3D array, not {.type {crps}}")
}

#' @rdname crps_uniqueness
inter_hd <- crps_uniqueness
