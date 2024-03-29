#' Random bit vector, matrix or array
#'
#' This function is a wrapper around [rbinom] to generate bit vectors. The matrix and array version are creating by row.
#'
#' @param size The size of the vector. Can be a list of dimensions to create a vector, matrix or array. If a vector larger than 3 is provided, each value is treated as the probability of obtaining 1 and a vector of bits is generated using a binomial distribution.
#' @param p Probability of obtaining a 1. By default it's `0.5`.
#' @param ... Extra arguments passed to matrix or array
#'
#' @returns The generated bit vector
#' @export
#'
#' @examples
#' ## Unbiased probabilities
#' rbits(10)
#'
#' ## Biased probabilities
#' rbits(10, p = 0.8)
#'
#' ## Matrix of bits
#' rbits(c(3, 4))
#'
#' ## 3D Array of bits
#' rbits(c(3, 4, 2))
#'
#' ## Individual probabilities
#' rbits(c(3, 3), runif(9, max = 0.5))
rbits <- function(size, p = 0.5, ...) {
  if (length(p) > 1 && prod(size) != length(p)) {
    stop("Number of probabilities does not match number of bits")
  }

  bits <- rbinom(prod(size), size = 1, prob = p)

  if (length(size) == 1) {
    bits
  } else if (length(size) == 2) {
    matrix(bits, nrow = size[[1]], ncol = size[[2]], byrow = TRUE, ...)
  } else if (length(size) == 3) {
    aperm(array(bits, dim = c(size[2], size[1], size[3]), ...), c(2, 1, 3))
  }
}

#' Hamming distance of two vectors
#'
#' @description
#' The Hamming distance of two vectors corresponds to the number of positions where the values differ.
#'
#' @details
#' NAs are discarded in any of the vectors by using `na.rm = TRUE` in `sum`.
#'
#' @param x A numeric or logical vector
#' @param y A numeric or logical vector
#' @param norm If `TRUE` normalize the distance to the vector length. By default it's `FALSE`
#'
#' @returns The Hamming distance
#'
#' @export
#' @examples
#' hamming_dist(c(0, 1, 0), c(0, 0, 0))
#' hamming_dist(c(0, 1, 0), c(0, 0, 0), norm = TRUE)
#'
#' ## NAs in any of the vectors are discarded
#' hamming_dist(c(1, NA, 3, 4, 5), c(1, 2, NA, 4, 5))
#' @export
hamming_dist <- function(x, y, norm = FALSE) {
  stopifnot(length(x) == length(y))
  hd <- sum(x != y, na.rm = TRUE)
  `if`(norm, hd / length(x[!is.na(x)]), hd)
}

#' Hamming weight of a binary vector
#'
#' @description
#' The Hamming weight is the number of non null symbols in a vector.
#' For a binary vector, it corresponds to the number of 1s.
#'
#' @details
#' NAs are discarded in the vector by using `na.rm = TRUE` in `sum`.
#'
#' @param v A logical or numeric vector
#' @param norm If `TRUE` (default is `FALSE`) normalize the vector
#'
#' @returns The Hamming weight
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
#' hamming_weight(c(1, 0, NA), norm = TRUE)
hamming_weight <- function(v, norm = FALSE) {
  weight <- sum(v, na.rm = TRUE)
  `if`(norm, weight / length(v[!is.na(v)]), weight)
}

#' Ratio of bits in a binary vector
#'
#' @description
#' The ratio is calculated as the number of 1s minus the number of 0s. A positive ratio indicates that there are more 1s than 0s, while a negative results indicates the opposite. `NA` are not accounted to calculate the length of the vector.
#'
#' @details
#' By using the [hamming_weight][pufr::hamming_weight] function, the ratio can be calculated in the following way.
#' \deqn{Ratio = \frac{HW(v) - (\#v - HW(v))}{\#v} = 2 \cdot HW_{norm}(v) - 1}
#' The operator \eqn{\#v} denotes the number of elements in the vector \eqn{v}.
#'
#' @param v A binary vector
#'
#' @returns The ratio of bits in the binary vector
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
#' ratio_bits(c(1, 1, 0, NA, NA))
ratio_bits <- function(v) {
  p_one <- hamming_weight(v, norm = TRUE)
  (2 * p_one) - 1
}

#' @rdname hamming_dist
#' @export
#' @examples
#' c(0, 1, 0) %HD% c(1, 0, 0)
"%HD%" <- function(x, y) hamming_dist(x, y, norm = FALSE)

#' @rdname hamming_dist
#' @export
#' @examples
#' c(0, 1, 0) %NHD% c(1, 0, 0)
"%NHD%" <- function(x, y) hamming_dist(x, y, norm = TRUE)

#' Compare a matrix by pairs of rows
#'
#' @description
#' Each pair of rows is compared using the given function. The pairs are chosen without repetition.
#'
#' @param m Vector of values.
#' @param fn Function that receives two row vectors.
#' @param ... Rest of arguments passed to `fn`.
#'
#' @returns List containing the results of applying the function to each pair of rows.
#' @export
#' @examples
#' #' Compare a matrix by pairs of rows
#' m <- rbits(c(5, 5))
#' res <- compare_pairwise(m, hamming_dist, norm = TRUE)
#' unlist(res)
#' length(res) == (5 * 4 / 2)
#'
#' ## Equivalence to uniqueness
#' res <- compare_pairwise(m, function(f, s) 1 - hamming_dist(f, s, norm = TRUE))
#' all(uniqueness(m) == unlist(res))
compare_pairwise <- function(m, fn, ...) {
  pairs <- lapply(seq_len(nrow(m) - 1), function(i) {
    len <- length(seq(i+1, nrow(m)))
    mapply(c, rep(i, len), seq(i + 1, nrow(m)), SIMPLIFY=FALSE)
  })
  pairs <- unlist(pairs, recursive = FALSE)

  lapply(pairs, function(p) fn(m[p[1], ], m[p[2], ], ...))
}


