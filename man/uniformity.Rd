% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formulas.R
\name{uniformity}
\alias{uniformity}
\title{Uniformity of a bit vector or a CRP matrix}
\usage{
uniformity(crps)
}
\arguments{
\item{crps}{A bit vector or a 2D CRP matrix}
}
\value{
If \code{crps} is a vector, the normalized Hamming weight. If \code{crps} is a matrix, the normalized Hamming weight of each row.
}
\description{
This function assumes that if the CRPs are supplied in a 2D matrix, each row corresponds to a device and each column corresponds to a CRP.
}
\details{
Uniformity measures the distribution of 1s and 0s across all CRPs for each device. To calculate uniformity, \code{margin} should be 1.

\deqn{Uniformity(d) = \frac{1}{\#C} \sum_{r \in R_d} r}
}
\examples{
## Uniformity of a vector
uniformity(rbits(50))

## Uniformity of a matrix
uniformity(rbits(c(10, 50)))
}
\seealso{
\link[=hamming_weight]{hamming_weight}
}
