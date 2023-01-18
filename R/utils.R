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
#' @export
hamming_dist <- function(x, y, norm = FALSE) {
  stopifnot(length(x) == length(y))
  hd <- sum(x != y, na.rm = TRUE)
  `if`(norm, hd / length(x), hd)
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
  `if`(norm, weight / length(v), weight)
}

#' Ratio of bits in a binary vector
#'
#' @description
#' The ratio is calculated as the number of 1s minus the number of 0s. A positive ratio indicates that there are more 1s than 0s, while a negative results indicates the opposite. `NA` by default are accounted to calculate the length of the vector. They can be discarded by using the argument `na.rm`.
#'
#' By using the [hamming_weight][pufr::hamming_weight] function, the ratio can be calculated in the following way.
#' \deqn{Ratio = \frac{HW(v) - (\#v - HW(v))}{\#v} = \frac{2 HW(v)}{\#v}- 1}
#' The operator \eqn{\#v} denotes the number of elements in the vector \eqn{v}.
#'
#' @param v A binary vector
#' @param na.rm If `TRUE` (default is `FALSE`) don't account `NA` for the length of the vector
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
#' ## `NA` are accounted for the length
#' ratio_bits(c(1, 1, 0, NA, NA))
#'
#' #' ## `NA` are discarded
#' ratio_bits(c(1, 1, 0, NA, NA), na.rm = TRUE)
ratio_bits <- function(v, na.rm = FALSE) {
  if (na.rm == TRUE) {
    bits <- v[!is.na(v)]
    n_ones <- hamming_weight(bits)
    size <- length(bits)
  } else {
    n_ones <- hamming_weight(v)
    size <- length(v)
  }
  return(((2 * n_ones) - size) / size)
}


#' @rdname hamming_dist
#' @export
#' @examples
#' c(0, 1, 0) %<>% c(1, 0, 0)
"%<>%" <- function(x, y) hamming_dist(x, y, norm = FALSE)

#' @rdname hamming_dist
#' @export
#' @examples
#' c(0, 1, 0) %</>% c(1, 0, 0)
"%</>%" <- function(x, y) hamming_dist(x, y, norm = TRUE)


#' Automatic parallelization of apply
#'
#' Use [parallel::parApply] if a parallel context has been created with [register_parallel]. Otherwise use [base::apply].
#'
#' @inheritParams base::apply
#' @seealso [register_parallel], [parallel::parApply]
#' @export
par_apply <- function(X, MARGIN, FUN, ...) {
  if (!is.null(pufr_env$ctx)) {
    parallel::parApply(pufr_env$ctx, X, MARGIN, FUN, ...)
  } else {
    apply(X, MARGIN, FUN, ...)
  }
}

#' Automatic parallelization of vapply
#'
#' #' Use [parallel::parApply] if a parallel context has been created with [register_parallel]. Otherwise use [base::apply].
#'
#' @inheritParams base::vapply
#' @seealso [register_parallel], [parallel::parSapply]
#' @export
par_vapply <- function(X, FUN, FUN.VALUE = NULL, ...) {
  if (!is.null(pufr_env$ctx)) {
    parallel::parSapply(pufr_env$ctx, X, FUN, ...)
  } else {
    vapply(X, FUN, FUN.VALUE, ...)
  }
}
