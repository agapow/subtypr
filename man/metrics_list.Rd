% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics_builder.R
\docType{data}
\name{metrics_list}
\alias{metrics_list}
\title{Metrics implemented}
\format{An object of class \code{list} of length 10.}
\usage{
metrics_list
}
\description{
subtypr provide both internal and external metrics to evaluate clustering
results.
}
\details{
There is two versions of each metric. The raw version - directly accessible
by the user - and a standardized version for the built-in functions of the
package (the idea is that the result is numeric, e.g. for silhouette index,
the value is the average silhouette width). The argument \code{metric} is always
the name of the standardized version.
You can access the standardized version with the
\code{get_metric} function. To have all the names of the standardized metrics,
use \code{names(get_metric())}.

Here we indicate the raw version documentation name and it's corresponding
standardized version: like this:
?raw_version ~ "name_of_standardized_version"

Internal metrics:
\itemize{
\item Silhouette index: ?silhouette_distance ~ "asw_distance" or
?silhouette_affinity ~ "asw_affinity"
}

External metrics:
\itemize{
\item Adjusted Rand Index: ?adjusted_rand_index ~ "ari"
\item Mutual Information: ?mutual_information ~ "mi"
\item Normalized Mutual Information: ?normalized_mutual_information ~ "nmi"
\item Meila's Variation Index: ?meila_vi ~ "meilaVI"
\item Dunn index: ?dunn_index ~ "dunn"
\item Homogeneity, Completeness & V-measure:
?homogeneity_completeness_vmeasure ~ "homogeneity" or "completeness" or
"v_measure"
}
}
\keyword{datasets}
