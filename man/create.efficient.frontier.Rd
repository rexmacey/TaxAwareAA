% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optbypairwiseiter.r
\name{create.efficient.frontier}
\alias{create.efficient.frontier}
\title{Create an efficient frontier using the pairwise iteration algorithm}
\usage{
create.efficient.frontier(cma, cma.ta, OptimizationParameters, pctassets,
  n.portfolios = 10)
}
\arguments{
\item{cma}{Capital Market Assumptions}

\item{cma.ta}{Capital Market Assumptions - tax adjusted}

\item{OptimizationParameters}{Optimization parameters}

\item{pctassets}{Percent of assets in each account}

\item{n.portfolios}{Number of points on the efficient frontier}
}
\value{
EFF object
}
\description{
Create an efficient frontier using the pairwise iteration algorithm
}
\examples{
create.eff.pairwise(cma, cma.ta, OptimizationParameters, pctassets, 10)
}
