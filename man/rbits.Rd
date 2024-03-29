% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rbits}
\alias{rbits}
\title{Random bit vector, matrix or array}
\usage{
rbits(size, p = 0.5, ...)
}
\arguments{
\item{size}{The size of the vector. Can be a list of dimensions to create a vector, matrix or array. If a vector larger than 3 is provided, each value is treated as the probability of obtaining 1 and a vector of bits is generated using a binomial distribution.}

\item{p}{Probability of obtaining a 1. By default it's \code{0.5}.}

\item{...}{Extra arguments passed to matrix or array}
}
\value{
The generated bit vector
}
\description{
This function is a wrapper around \link{rbinom} to generate bit vectors. The matrix and array version are creating by row.
}
\examples{
## Unbiased probabilities
rbits(10)

## Biased probabilities
rbits(10, p = 0.8)

## Matrix of bits
rbits(c(3, 4))

## 3D Array of bits
rbits(c(3, 4, 2))

## Individual probabilities
rbits(c(3, 3), runif(9, max = 0.5))
}
