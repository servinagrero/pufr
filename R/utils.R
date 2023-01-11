#' Random binary vector
#'
#' This function is a wrapper around [sample] to generate binary vectors.
#'
#' @param size The size of the vector
#' @param prob A numeric vector with 2 values for the probabilities of `0` and `1` respectively. By default the probabilities are `0.5` each.
#'
#' @return The generated binary vector
#' @export
#'
#' @examples
#' ## Unbiased probabilities
#' rbits(10)
#'
#' ## Biased probabilities
#' rbits(10, prob = c(0.1, 0.9))
rbits <- function(size, prob = NULL) {
  sample(c(0, 1), size, replace = TRUE, prob)
}

#' Ratio of bits in a binary vector
#'
#' @description
#' The ratio is calculated as the number of 1s minus the number of 0s. A positive ratio indicates that there are more 1s than 0s, while a negative results indicates the opposite.
#'
#' By using the [hamming_weight][pufr::hamming_weight] function, the ratio can be calculated in the following way.
#' \deqn{Ratio = \frac{HW(v) - (\#v - HW(v))}{\#v} = \frac{2 HW(v)}{\#v}- 1}
#' The operator \eqn{\#v} denotes the number of elements in the vector \eqn{v}.
#'
#' @param v A logical or numeric vector
#'
#' @return The ratio of bits in the binary vector
#'
#' @export
#' @seealso [hamming_weight][pufr::hamming_weight]
#' @examples
#' ## Negative ratio
#' ratio_bits(c(0, 1, 0))
#'
#' ## Positive ratio
#' ratio_bits(c(1, 1, 0))
#'
#' ## `NA` are discarded
#' ratio_bits(c(1, 0, NA))
ratio_bits <- function(v) {
  n_ones <- hamming_weight(v)
  ((2 * n_ones) - length(v)) / length(v)
}

#' Hamming weight of a binary vector
#'
#' @description
#' The Hamming weight is the number of non null symbols in a vector.
#' For a binary vector, it corresponds to the number of 1s.
#'
#' @param v A logical or numeric vector
#' @param norm If `TRUE` (default is `FALSE`) normalize the vector
#'
#' @return The Hamming weight
#'
#' @export
#' @examples
#' ## Weight of the vector
#' hamming_weight(c(0, 1, 1))
#'
#' ## Normalized weight
#' hamming_weight(c(0, 1, 1), norm = TRUE)
#'
#' ## `NA` are discarded
#' hamming_weight(c(1, 0, NA))
hamming_weight <- function(v, norm = FALSE) {
  weight <- sum(v, na.rm = TRUE)
  if (norm == TRUE) {
    return(weight / length(v))
  } else {
    return(weight)
  }
}
