library(ggplot2)
library(dplyr)
library(cowplot)
library(viridis)

#' Compute the quality metrics of the given CRP matrix
#'
#' @param crps A 2D or 3D CRP matrix
#' @param with_entropy If `TRUE`, wrap the results of the metrics with the Shannon entropy. By default `FALSE`.
#'
#' @returns The calculated metrics in a `pufmetrics` object.
#'
#' @export
#' @examples
#' crps <- rbits(c(5, 50))
#' metrics(crps)
#'
#' crps <- rbits(c(5, 50, 3))
#' metrics(crps)
metrics <- function(crps, with_entropy = FALSE) {
  m <- structure(list(reliability = NA), class = "pufmetrics")

  m$devices <- `if`(is.null(rownames(crps)), seq_len(nrow(crps)), rownames(crps))
  m$challenges <- `if`(is.null(colnames(crps)), seq_len(ncol(crps)), colnames(crps))

  if (is.matrix(crps)) {
    m$uniformity <- as.vector(pufr::uniformity(crps))
    m$bitaliasing <- as.vector(pufr::bitaliasing(crps))
    m$uniqueness <- pufr::uniqueness(crps)
  } else {
    m$samples <- `if`(is.null(dimnames(crps)[3]), seq_len(dim(crps)[3]), dimnames(crps)[[3]])
    m$uniformity <- lapply(m$samples, function(s) as.vector(uniformity(crps[, , s])))
    m$bitaliasing <- lapply(m$samples, function(s) as.vector(bitaliasing(crps[, , s])))
    m$uniqueness <- lapply(m$samples, function(s) as.vector(uniqueness(crps[, , s])))
    m$reliability <- pufr::reliability(crps)
  }
  return(`if`(with_entropy, with_entropy(m), m))
}


#' Convert metrics to entropy calculation
#'
#' @param metrics A pufmetrics object
#'
#' @returns The new pufmetrics object where the metrics have been passed to `entropy_p` description
#'
#' @seealso [entropy_p][pufr::entropy_p]
#' @export
#' @examples
#' crps <- rbits(c(5, 50))
#' m <- metrics(crps)
#' m
#' with_entropy(m)
with_entropy <- function(metrics) {
  stopifnot(class(metrics) == "pufmetrics")

  m <- metrics

  if (dim(metrics)[3] == 1) {
    m$uniformity <- entropy_p(metrics$uniformity)
    m$bitaliasing <- entropy_p(metrics$bitaliasing)
  } else {
    m$uniformity <- lapply(metrics$uniformity, entropy_p)
    m$bitaliasing <- lapply(metrics$bitaliasing, entropy_p)
  }
  return(m)
}

#' Create a plot with a summary of all the metrics
#'
#' If the metrics come from a 2D CRP table, 3 histograms are created for uniformity, bitaliasing and uniqueness.
#'
#' In the case of a 3D CRP table, 4 graphs are created, 3 histograms for uniformity, bitaliasing and uniqueness, combining all samples, and a matrix showing the reliability of each response.
#'
#' @param x The PUF metrics
#' @param ... Additional parameters
#'
#' @export
#' @examples
#' ## With a single sample
#' crps <- rbits(c(5, 50))
#' plot(metrics(crps))
#'
#' ## With multiple samples
#' crps <- rbits(c(5, 50, 3))
#' plot(metrics(crps))
plot.pufmetrics <- function(x, ...) {
  histogram <- function(x) {
    df <- do.call(rbind, x) %>%
      reshape2::melt() %>%
      rename(sample = "Var1")
    ggplot(df) +
      geom_histogram(
        aes(x = value, group = sample, fill = as.factor(sample)),
        position = "dodge", bins = 10
      ) +
      scale_fill_viridis_d() +
      scale_x_continuous(n.breaks = 10) +
      coord_cartesian(xlim = c(0, 1)) +
      labs(fill = "Sample")
  }

  if (is.matrix(x$reliability)) {
    p_unif <- histogram(x$uniformity)
    p_ba <- histogram(x$bitaliasing)
    p_uniq <- histogram(x$uniqueness)

    p_rel <- reshape2::melt(x$reliability) %>%
      ggplot() +
      geom_raster(aes(x = Var2, y = Var1, fill = value)) +
      scale_fill_viridis() +
      labs(x = "Challenge", y = "Device", fill = "Rel")

    cowplot::plot_grid(
      p_unif,
      p_ba,
      p_uniq,
      p_rel,
      labels = c("Uniformity", "Bitaliasing", "Uniqueness", "Reliability"),
      ncol = 2, nrow = 2
    )
  } else {
    plist <- lapply(
      x[c("uniformity", "bitaliasing", "uniqueness")],
      function(mat) {
        data.frame(value = mat) %>%
          ggplot() +
          geom_histogram(aes(x = value), bins = 10) +
          scale_x_continuous(n.breaks = 10) +
          coord_cartesian(xlim = c(0, 1))
      }
    )

    cowplot::plot_grid(
      plotlist = plist,
      labels = c("Uniformity", "Bitaliasing", "Uniqueness"),
      ncol = 1
    )
  }
}

#' Returns the dimensions of the metrics
#'
#' The dimensions are defined as the number of devices, number of challenges and number of samples. When only 1 sample has been used, the 3rd dimension defaults to 1.
#'
#' @param x The PUF metrics
#'
#' @returns The number of devices, challenges and samples from the metrics
#'
#' @export
#' @examples
#' crps <- rbits(c(5, 50))
#' dim(metrics(crps))
#'
#' crps <- rbits(c(5, 50, 3))
#' dim(metrics(crps))
dim.pufmetrics <- function(x) {
  if (is.matrix(x$reliability)) {
    c(nrow(x$reliability), ncol(x$reliability), length(x$uniformity))
  } else {
    c(length(x$uniformity), length(x$bitaliasing), 1)
  }
}
