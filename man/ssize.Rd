% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssize.R
\name{ssize}
\alias{ssize}
\title{Size of objects in slots of S4 object}
\usage{
ssize(x, recursive = TRUE, depth = Inf)
}
\arguments{
\item{x}{An S4 object}

\item{recursive}{A logical(1) indicating whether the function should
recursively be called on slots that are themselves S4 objects}

\item{depth}{An numeric(1) indicating how many levels to recurse}
}
\value{
A nested named list with the size of the objects in each slot
}
\description{
Size of objects in slots of S4 object
}
