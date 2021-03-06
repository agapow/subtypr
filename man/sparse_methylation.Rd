% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparse_methylation.R
\docType{data}
\name{sparse_methylation}
\alias{sparse_methylation}
\title{Sparse data to test tools}
\format{It's a samples x feature matrix.}
\usage{
sparse_methylation
}
\description{
This is the methylation data from \code{gbm_data}, see \code{?gbm_data} but with
missing values. We removed a variable percentage of features per sample.
This variable percentage was generated based on a gaussian distribution
centered on 0.12, with a standard deviation of 0.08. We set 0 for the
negative values.
}
\keyword{datasets}
