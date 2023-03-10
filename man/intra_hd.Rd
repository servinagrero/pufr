% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{intra_hd}
\alias{intra_hd}
\title{Intra Hamming distance of CRPs}
\usage{
intra_hd(crps, ref_sample = 1)
}
\arguments{
\item{crps}{A binary 2D matrix or 3D array}

\item{ref_sample}{Row of the 2D matrix used as reference CRPs}
}
\value{
If \code{crps} is a 2D matrix, a vector of size \code{nrow(crps) - 1}. If \code{crps} is a 3D array, a 2D matrix where each row contains the intra Hamming distance of the samples for that device.
}
\description{
The intra Hamming distance of two sets of CRPs measures the numbers of CRPs that differ. N sets of CRPs are equal if their intra Hamming distances are 0.

A response is reliable if it does not change in time, thus, its intra Hamming distance is equal to 0.

If \code{crps} is a 2D matrix, it is assumed that each row corresponds to a sample and each column to a CRP. The \code{ref_sample} will say which row is the sample taken as reference.
In the case of a 3D matrix, each row corresponds to a device, each column to a CRP and each 3rd dimension to a different sample.

The order of the samples is calculated as \code{setdiff(seq_len(nsamples), ref_sample)} where nsamples corresponds to \code{nrow(crps)} in the case of a 2D matrix and \code{dim(crps)[3]} in the case of a 3D matrix.
}
\examples{
## Set of CRPs
mat <- matrix(0, nrow = 10, ncol = 50)
intra_hd(mat, 1)

## Set of devices with their respective samples
mat <- array(rbits(10 * 50 * 3), dim = c(10, 50, 3))
intra_hd(mat)
}
\seealso{
\link[=hamming_dist]{hamming_dist}
}
