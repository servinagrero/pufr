% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{hamming_weight}
\alias{hamming_weight}
\title{Hamming weight of a binary vector}
\usage{
hamming_weight(v, norm = FALSE)
}
\arguments{
\item{v}{A logical or numeric vector}

\item{norm}{If \code{TRUE} (default is \code{FALSE}) normalize the vector}
}
\value{
The Hamming weight
}
\description{
The Hamming weight is the number of non null symbols in a vector.
For a binary vector, it corresponds to the number of 1s.
}
\details{
NAs are discarded in the vector by using \code{na.rm = TRUE} in \code{sum}.
}
\examples{
## Weight of the vector
hamming_weight(c(0, 1, 1))

## Normalized weight
hamming_weight(c(0, 1, 1), norm = TRUE)

## `NA` are discarded
hamming_weight(c(1, 0, NA), norm = TRUE)
}
