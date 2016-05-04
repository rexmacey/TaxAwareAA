#' TaxAwareAA: A package for performing tax-aware asset allocation.
#'
#' TaxAwareAA attempts to both allocate and locate assets in a mean-variant
#' efficient manner. Its main functions include a function to create tax
#' efficient, efficient frontiers and to create a portfolio with a target
#' standard deviation that is efficient using resampling. These are
#' efficient.frontier and resample.target.risk.
#'
#' To perform a tax aware asset allocation we need to have a set of assumptions
#' about the assets (also called asset classes in this documentation).  These
#' set of assumptions are put in an object called a cma (capital market
#' assumptions). For clarity, we refer to the base, non-tax-adjusted set of
#' capital market assumptions as a cma.  If tax-adjusted we will use cma.ta. We
#' also need to know something about the investor.  We use an object called
#' investor.  We can combine a cmf and an investor into a tax-aware capital
#' market assumption object, the cma.ta.
#'
#' We assume a set of returns and covariance matrix for a set of asset classes
#' just as we would in a non-tax-aware exercise. Additional information on the
#' nature of the asset classes is required so that we can calculate the impact
#' of taxes.  For example, we need the yield on each asset and the character of
#' the income it generates (ordinary income, tax-free income, qualified
#' dividend).
#'
#' We assume the investor has assets in three types of accounts: \describe{
#' \item{Taxable}{An account in which income and capital gains are subject to
#' tax. Example, a brokerage account for an individual.} \item{Deferred}{An
#' account that is taxed when the assets are withdrawn. Example, an IRA.}
#' \item{Exempt}{An account which is not subject to tax. Example, a Roth IRA} }
#' We know amounts the investor has in each type of account. Further, we know
#' the rates at which income, capital gains and qualified dividends are taxed.
#'
#' To create a tax-aware cma, we triple the number of asset classes of the base
#' cma. We would have each of the base asset classes in each of the three
#' account types and treat them separately. So if there were just bonds and
#' stocks in the base cma, in a tax-aware cma we have six asset classes.
#'
#' The (after-tax) returns of the assets differ depending on their location.
#' There are arguments that the variance of the assets should also change, but
#' this version does not address that.  The argument for adjusting the risk is
#' that in the face of taxes, the government reduces the gain (or loss) thus
#' reducing the variability.  It is the author's opinion that covariance
#' matrices, especially generated on historical data, understate the risk which
#' most investor's care about which is downside risk in a crisis.  Historically
#' based covariance underestimates the higher volatility and correlations that
#' often occur in a crisis.  This is something to consider in future versions.
#' The author has chosen to address this in the future.  Since the user
#' specifies tne covariance matrix, the user may supply one that considers the
#' impact of taxes.
#'
#' This tax-aware optimization is similar to a non-tax aware.  We use 3x the
#' number of assets (N). The first N assets are those in the taxable account.
#' The second N are in the deferred. The last N are in the exempt account.  We
#' add constraints so the the weights in each match the proportions of the
#' assets by account type.  The user may specify constraints by base asset
#' class.  For example, if the first asset clas has a maximum weight of 25%,
#' then this model introduces the constraint that the sum of the weights of
#' class [1] + class [N+1] + class [2N+1] <= 0.25
#'
#' The results is a set of weights across the 3N asset classes.
#'
#' About the Investor
#'
#' @docType package
#' @name TaxAwareAA
#' @author Rex Macey, \email{rex@macey.us}
#' @keywords asset allocation portfolio
#'
NULL
