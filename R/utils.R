#' Random binary vector
#'
#' This function is a wrapper around [sample] to generate binary vectors.
#'
#' @param size The size of the vector
#' @param p Probability of obtaining a 1. By default it's `0.5`.
#' @param ... Extra arguments passed to matrix or array
#'
#' @return The generated binary vector
#' @export
#'
#' @examples
#' ## Unbiased probabilities
#' rbits(10)
#'
#' ## Biased probabilities
#' rbits(10, p = 0.8)
rbits <- function(size, p = 0.5, ...) {
  bits <- sample(c(0, 1), prod(size), replace = TRUE, c(1 - p, p))
  switch (length(size),
    "1" = bits,
    "2" = matrix(bits, nrow = size[[1]], ncol = size[[2]], ...),
    "3" = array(bits, dim = size, ...)
  )
}

#' Hamming distance of two vectors
#'
#' @description
#' The Hamming distance of two vectors corresponds to the number of positions where the values differ.
#'
#' @details
#' NAs are discarded in any of the vectors.
#'
#' @param x A numeric or logical vector
#' @param y A numeric or logical vector
#' @param norm If `TRUE` normalize the distance to the vector length. By default it's `FALSE`
#'
#' @return The Hamming distance
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
#' NAs are discarded in the vector.
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

#' Automatic parallelization of apply
#'
#' Use [parallel::parApply] if a parallel context has been created with [register_parallel]. Otherwise use [base::apply].
#'
#' @param v Vector of values
#' @param margin Vector giving the subscripts which the function will be applied over. E.g., for a matrix 1 indicates rows, 2 indicates columns, c(1, 2) indicates rows and columns. Where X has named dimnames, it can be a character vector selecting dimension names.
#' @param fn Function to apply.
#' @param ... Rest of arguments passed to `fn`.
#'
#' @returns Vector or array or list of values obtained by applying a function to margins of an array or matrix.
#' @seealso [register_parallel], [parallel::parApply], [apply]
#' @export
par_apply <- function(v, margin, fn, ...) {
  if (!is.null(pufr_env$ctx)) {
    parallel::parApply(pufr_env$ctx, v, margin, fn, ...)
  } else {
    apply(v, margin, fn, ...)
  }
}

#' Automatic parallelization of vapply
#'
#' Use [parallel::parApply] if a parallel context has been created with [register_parallel]. Otherwise use [base::apply].
#'
#' @param v Vector of values.
#' @param fn Function to apply.
#' @param value Expected type of value.
#' @param ... Rest of arguments passed to `fn`.
#'
#' @returns Vector or array or list of values obtained by applying a function to margins of an array or matrix.
#' @seealso [register_parallel], [parallel::parSapply], [vapply]
#' @export
par_vapply <- function(v, fn, value = NULL, ...) {
  if (!is.null(pufr_env$ctx)) {
    parallel::parSapply(pufr_env$ctx, v, fn, ...)
  } else {
    vapply(v, fn, value, ...)
  }
}

#' Compare a matrix by pairs of rows
#'
#' @description
#' Each pair of rows is compared using the given function.
#' The pairs are chosen without repetition.
#'
#' @param m Vector of values.
#' @param fn Function that receives two rows.
#' @param ... Rest of arguments passed to `fn`.
#'
#' @returns Vector containing the results of all comparisons.
#' @export
#' @examples
#' #' Compare a matrix by pairs of rows
#' m <- matrix(rbits(25), 5, 5)
#' res <- compare_pairwise(m, hamming_dist, norm = TRUE)
#' res
#' length(res) == (5 * 4 / 2)
#'
#' ## Equivalence to uniqueness
#' res <- compare_pairwise(m, function(f, s) 1 - hamming_dist(f, s, norm = TRUE))
#' all(uniqueness(m) == res)
compare_pairwise <- function(m, fn, ...) {
  rows <- nrow(m)
  # TODO: Usne RcppAlgos::comboGeneral to create the pairs
  res <- sapply(seq(1, rows - 1), function(x) seq(x + 1, rows, 1))
  idx <- mapply(c, unlist(res), rep(seq(1, rows - 1), seq(rows - 1, 1)), SIMPLIFY = TRUE)
  par_apply(idx, 2, function(p) {
    fn(m[p[1], ], m[p[2], ], ...)
  })
}
