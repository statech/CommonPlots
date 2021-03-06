% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_utils.R
\name{n_nna}
\alias{n_nna}
\title{Number of non-NAs}
\usage{
n_nna(vec)
}
\arguments{
\item{vec}{Vector: a vector-like object}
}
\value{
Integer: number of non-NAs
}
\description{
Counts the number of elements that are not \code{NA} in a vector
}
\examples{
n_nna(c(NA, 9, 10))
n_nna(c('b', 'c', NA))
}
