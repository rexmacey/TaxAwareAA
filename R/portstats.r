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
    pvar<-portvar(w,cma.ta)
    return(as.numeric(sum(w*cma$ret))-pvar/2)
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
#' Calculates a measure of diversification
#' 
#' This score is scaled such that if one equally invested in n assets, the score will be n.  Thus higher values indicate greater diversification.
#' This does not incorporate any correlation among asset classes. It treats each asset class independently.
#' 
#' @param w Weights of assets in portfolio
#' @return Diversification score.  This score is scaled such that if one equally invested in n assets, the score will be n.  
#' @export
#' 
diversification_score<-function(w){
    out<-1/sum(w^2)
    return(out)
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

#' Calculate the future after-tax value of a portfolio
#' 
#' The returns in the cma are assumed to be arithmetic.  This adjusts them to geometric before compounding. 
#'
#' @param w Weights of the portfolio
#' @param cma.ta Tax-aware capital market assumptions
#' @param horizon Horizon over which to calculate return
#' @return After-tax value
#' @export
#' 
calc.future.at.value<-function(w,cma.ta,horizon){
    pret<-portret(w,cma.ta)
    prisk<-portrisk(w,cma.ta)
    pvar<-portvar(w,cma.ta)
    out<-sum(cma.ta$account.values)*(1+pret-pvar/2)^horizon
}

#' Return using the square-root-of-T rule
#' 
#' This calculation follows that described on page 16 of RAFI AA-Asset_Class-Risk.pdf document.
#'
#' @param p probability of doing worse than the result
#' @param ExpRetAnnual Annualized expected return
#' @param SD  Annualized standard deviation
#' @param T Time in years
#' @param method (unused)
#'
#' @return value Annualized return with at least a probability of p of occurring.
#' @export
#'
#' @examples ret_SqRootofT(0.025,1.3,14.3,10)
ret_SqRootofT<-function(p,ExpRetAnnual,SD,T,method="RAFI"){
    z<-qnorm(p,0,1)
    out<-ExpRetAnnual+z*SD/(2*sqrt(T)) #per page 16 of RAFI AA-Asset_Class-Risk.pdf
    return(out)
}
