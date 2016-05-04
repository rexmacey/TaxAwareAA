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

#' cma is a list that must have
#' classes A list of class names
#' nclasses
