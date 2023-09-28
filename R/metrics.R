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
metrics <- function(crps, with_entropy = FALSE, ...) {
  m <- structure(list(reliability = NA), class = "pufmetrics")

  if (is.matrix(crps)) {
    m$uniformity <- pufr::uniformity(crps)
    m$bitaliasing <- pufr::bitaliasing(crps)
    m$uniqueness <- pufr::uniqueness(crps)
  } else {
    samples <- seq_len(dim(crps)[3])
    m$uniformity <- lapply(samples, function(s) uniformity(crps[, , s]))
    m$bitaliasing <- lapply(samples, function(s) bitaliasing(crps[, , s]))
    m$uniqueness <- lapply(samples, function(s) uniqueness(crps[, , s]))
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
#' @seealso [entropy_p()][pufr::entropy_p]
#' @export
with_entropy <- function(metrics) {
  stopifnot(class(metrics) == "pufmetrics")

  m <- metrics

  if (length(dim(metrics)) == 2) {
    m$uniformity <- entropy_p(metrics$uniformity)
    m$bitaliasing <- entropy_p(metrics$bitaliasing)
  } else {
    m$uniformity <- lapply(metrics$uniformity, entropy_p)
    m$bitaliasing <- lapply(metrics$bitaliasing, entropy_p)
  }
  m
}

#' Create a plot with a summary of all the metrics
#'
#' If the metrics come from a 2D CRP table, 3 histograms are created for uniformity, bitaliasing and uniqueness.
#'
#' In the case of a 3D CRP table, 4 graphs are created, 3 histograms for uniformity, bitaliasing and uniqueness, combining all samples, and a matrix showing the reliability of each response.
#'
#' @param m The PUF metrics
#' @param ... Additional parameters
#'
#' @export
plot.pufmetrics <- function(m, ...) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))

  histogram <- function(m) {
    do.call(rbind, m) %>%
      reshape2::melt() %>%
      rename(sample = "Var1") %>%
      ggplot() +
      geom_histogram(
        aes(x = value, group = sample, fill = as.factor(sample)),
        position = "dodge", bins = 10
      ) +
      scale_fill_viridis_d() +
      scale_x_continuous(n.breaks = 10) +
      coord_cartesian(xlim = c(0, 1)) +
      labs(fill = "Sample")
  }

  if (is.matrix(m$reliability)) {
    p_unif <- histogram(m$uniformity)
    p_ba <- histogram(m$bitaliasing)
    p_uniq <- histogram(m$uniqueness)

    p_rel <- reshape2::melt(m$reliability) %>%
      ggplot() +
      geom_raster(aes(x = Var1, y = Var2, fill = value)) +
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
      m[c("uniformity", "bitaliasing", "uniqueness")],
      function(met) {
        data.frame(value = met) %>%
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
#' @param m The PUF metrics
#' @param ... Additional parameters
#'
#' @export
dim.pufmetrics <- function(m, ...) {
  samples <- `if`(is.matrix(m$reliability), length(m$uniformity), 1)
  c(nrow(m$reliability), ncol(m$reliability), samples)
}

#' #' @export
#' report <- function(m, ...) {
#'   cli::cli_rule("PUF Metrics")
#'   cli::cli_text("Number of devices: {dim(m)[1]}")
#'   cli::cli_text("Number of challenges: {dim(m)[2]}")
#'   if (length(dim(m)) == 3) {
#'     cli::cli_text("Number of samples: {dim(m)[3]}")
#'   }
#' }
