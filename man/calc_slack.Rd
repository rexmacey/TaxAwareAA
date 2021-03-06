% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optbypairwiseiter.r
\name{calc_slack}
\alias{calc_slack}
\title{Calculates slack}
\usage{
calc_slack(x, boxmin, boxmax, maxdelta)
}
\arguments{
\item{x}{Portfolio weights}

\item{boxmin}{Box min constraints}

\item{boxmax}{Box max constraings}

\item{maxdelta}{Maximum amount by which to change an asset class}
}
\value{

}
\description{
Calculates weights that can be added or subtracted from a portfolios without violating box contraints.
Also returns the indices representing which asset classes may be increased and decreased
}
\examples{
calc_slack(x, boxmin, boxmax, 0.03)
}
