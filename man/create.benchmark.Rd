% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optbypairwiseiter.r
\name{create.benchmark}
\alias{create.benchmark}
\title{Expands an abbreviated (short) benchmark into a longer definition}
\usage{
create.benchmark(benchmark.txt, cma, cma.ta, investor,
  OptimizationParameters)
}
\arguments{
\item{benchmark.txt}{Benchmark in text format (e.g. "Munis=0.6,USLarge=0.4")}

\item{cma}{CMA object}

\item{cma.ta}{CMA.ta object}

\item{investor}{Investor object}
}
\value{
List with two items: wts.bench and wts.bnech.ta
}
\description{
The Control.xlsx file allows the specification of a benchmark using only the non-zero asset classes. This function 
takes that definition as input and creates an expanded definition including the classes with zero weights both
pretax and aftertax
}
\examples{
expand.benchmark(short.benchmark, cma, cma.ta, investor)
}
