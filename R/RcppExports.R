# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Hamming distance of two vectors
#'
#' @param x A numeric or logical vector
#' @param y A numeric or logical vector
#' @param norm If `TRUE` (default is `FALSE`) normalize the distance to the vector length
#'
#' @return The hamming distance
#'
#' @export
#' @examples
#' hamming_dist(c(0, 1, 0), c(0, 0, 0))
#' hamming_dist(c(0, 1, 0), c(0, 0, 0), norm = TRUE)
hamming_dist <- function(x, y, norm = FALSE) {
    .Call(`_pufr_hamming_dist`, x, y, norm)
}

#' Uniqueness of CRPs
#'
#' The uniqueness is calculated as the average of the hamming distance of the CRPs of two devices, for every pair of devices.
#'
#' The number of pairs of devices is calculated as:
#' \deqn{N = \binom{D}{2} = \frac{D!}{2 \cdot (D - 2)!}}
#' where D represents the number of devices.
#'
#' @param crps A logical or numeric matrix
#'
#' @return The uniqueness of the CRP table
#'
#' @export
#' @examples
#' mat <- matrix(sample(c(0, 1, 100, replace = TRUE)), nrow = 10, ncol = 10)
#' crps_uniqueness(mat)
crps_uniqueness <- function(crps) {
    .Call(`_pufr_crps_uniqueness`, crps)
}

