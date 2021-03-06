% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{pre_process}
\alias{pre_process}
\title{Pre-process feature matrices before analysis.}
\usage{
pre_process(m, method = c("center", "scale"), ...)
}
\arguments{
\item{m}{a feature matrix with samples in rows and features in columns.}

\item{method}{list of preprocessing methods as defined in caret::preProcess,
centering and scaling it by default. See \code{\link[caret]{preProcess}}.}

\item{...}{preprocessing methods parameters as required by caret::preProcess}
}
\value{
the preprocessed data.
}
\description{
Often, data must be adjusted (scaled, imputed, etc.) before it can be
analysed.  This method presents a simple and consistent interface for the same.
}
\details{
Internally this is a wrapper of the equivalent methods from the
\href{https://cran.r-project.org/web/packages/caret/index.html}{caret}
package, that simplifies and streamlines usage. Note that while it suits
our purposes to have a single step to adjust the data, caret package
presents this as two steps - assess preprocessing and then carry it out.
This is because you may need to calculate preprocessing over the training
data and then transform the training & test data in the same way, or where
you wish to explicitly capture the preprocessing stage (e.g. PCA).
}
