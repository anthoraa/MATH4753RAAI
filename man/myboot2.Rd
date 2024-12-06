% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{Bootleg Function 2}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{number of iterations}

\item{x}{vector of data}

\item{fun}{statistic to be calculated}

\item{alpha}{1 - alpha is confidence interval}

\item{cx}{size of text label}

\item{...}{}
}
\value{
list of ci, function, x, and statistic, and a plot
}
\description{
Bootleg Function 2
}
\examples{
myboot2(iter = 10000, rnorm(20, 5, 5), fun="mean", alpha = 0.05)
}
