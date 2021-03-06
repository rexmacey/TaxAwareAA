% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rafi.r
\name{pretax_return}
\alias{pretax_return}
\title{Calculates the pre-tax return}
\usage{
pretax_return(yld, growth, valchange, horizon = 10)
}
\arguments{
\item{yld}{is initial yield in decimal (e.g. 0.03 for 3 percent)}

\item{growth}{is the annual growth rate of the income in decimal.}

\item{valchange}{is the annual percentage change in the valuation in decimal}

\item{horizon}{number of years to simulate}
}
\value{
Pretax annual return in decimal
}
\description{
Simulates the investment value without taxes
}
