% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_utils.R
\name{unique_na}
\alias{unique_na}
\title{Extract Unique Non-NA Elements}
\usage{
unique_na(obj, ...)
}
\arguments{
\item{obj}{Object: a vector or a data frame or an array or NULL}

\item{...}{Ellipsis: arguments passed to \code{\link[base]{unique}}}
}
\value{
See return of \code{\link[base]{unique}}
}
\examples{
unique_na(c(NA, 9, 10))
unique_na(c('b', 'c', NA))
}
\seealso{
\code{\link[base]{unique}}
}
