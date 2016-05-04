#' Calculate the variance of a portfolio
#'
#' This function is called by the portrisk function
#'
#' @param w Weights of the assets in a portfolio
#' @param cma Capital market assumptions.  Must have a cov item that is a square
#'   matrix each dimension matching the length of w
#' @export
#' @return value representing the variance of the portfolio
#'
portvar<-function(w,cma){
    return(as.numeric(t(w) %*% cma$cov %*% w))
}

#' Calculate the return of a portfolio
#'
#' This is the expected return given the capital market assumptions
#'
#' @param w Weights of the assets in a portfolio
#' @param cma Capital market assumptions.  Must have a ret item with a length
#'   the length of w
#' @export
#' @return value representing the variance of the portfolio
#'
portret<-function(w,cma){
    return(as.numeric(sum(w*cma$ret)))
}

#' Calculate the standard deviation of a portfolio
#'
#' This function calls the portrisk function
#'
#' @param w Weights of the assets in a portfolio
#' @param cma Capital market assumptions.  Must have a cov item that is a square
#'   matrix each dimension matching the length of w
#' @export
#' @return value representing the standard deviation of the portfolio
#'
portrisk<-function(w,cma){
    return(as.numeric(portvar(w,cma)^.5))
}

#' Calculate the asset class weights across account types
#'
#' Tax-aware optimization produces weights for each asset class in each account type.  This function returns the weight of each asset class
#' across all acount types (e.g. sum of cash in taxable, deferred and exempt accounts)
#'
#' @param w Weights of the assets in a portfolio
#' @param cma.ta Tax-aware capital market assumptions.  
#' @export
#' @return value Vector of asset class weights
#'
calc.ac.wts<-function(w,cma.ta){
    out<-w[1:(cma.ta$base.nclasses)]+w[(cma.ta$base.nclasses+1):(2*cma.ta$base.nclasses)]+
                         w[(2*cma.ta$base.nclasses+1):(3*cma.ta$base.nclasses)]
    names(out)<-cma.ta$base.classes
    return(out)
}