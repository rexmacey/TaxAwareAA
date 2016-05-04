#' Create an efficient frontier
#'
#' This function creates a mean-variant efficient frontier given a set of
#' capital market assumptions.
#'
#' @param cma.ta Tax-aware capital market assumptions
#' @param n.portfolios Number of portfolios (points) to produce on the frontier.
#'   Output will have one less point if the minimum return portfolio is
#'   inefficient.
#' @keywords asset allocation efficient frontier
#' @export
#' @return A matrix. Each row is a point on the frontier.  The first column is
#'   the return. The second column is the standard deviation. Subsequent columns
#'   are asset class weights.
#'
efficient.frontier<-function(cma.ta,n.portfolios=25){
    nclasses<-cma.ta$nclasses
    minmax<-find.minmax.return(cma.ta)
    target.ret.vec<-seq(minmax$min$opt,minmax$max$opt,length.out = n.portfolios)
    target.ret.vec[1]<-target.ret.vec[1]+.00001 # a fudge to make sure the optimize.target.return function can find a solution
    temp<-lapply(target.ret.vec,optimize.target.return,cma.ta=cma.ta)
    out<-matrix(0,nrow=n.portfolios,ncol=2+nclasses)
    colnames(out)<-c("Return","Risk",cma.ta$classes)

    out[,1]<-target.ret.vec
    out[,3:(2+nclasses)]<-t(sapply(seq(1:n.portfolios),function(x) temp[[x]]$solution))
    out[1:n.portfolios,2]<-sapply(seq(1:n.portfolios),function(x) portrisk(out[x,3:(2+nclasses)],cma.ta))
    #remove first point if inefficient
    if (out[1,"Risk"]>=out[2,"Risk"]) out<-out[2:n.portfolios,]
    rownames(out)<-paste0("EffPt",seq(1,nrow(out)))
    return(out)
}

#'  Find Min and Max Return
#'
#'  This function finds the minimum and maximum feasible returns given a set of
#'  capital market assumptions, including constraints. It is called to find the
#'  endpoints for creating an efficient frontier. It returns a list with min and
#'  max items each of which are produced by solveLP. The opt value is the min or
#'  max respectively. Requires linprog.
#'
#'  @param cma.ta Tax-aware capital market assumptions
#'  @keywords tax-aware efficient frontier
#'  @return A two item list containing min and max each of which are produced by
#'    solveLP. The opt value is the min or max return respectively
#'  @export
#'  @seealso \code{\link[linprog]{solveLP}}
#'  @keywords find.minmax.return()
#'
find.minmax.return<-function(cma.ta){ # return min and max expected returns that meet constraints
    require(linprog)
    nclasses.cmf<-cma.ta$base.nclasses
    nclasses<-cma.ta$nclasses

    ret<-cma.ta$ret
    objVec <- ret

    account.values<-cma.ta$account.values
    nconstraints.cmf<-length(cma.ta$constraints)
    cmf.cons.mat<-matrix(unlist(cma.ta$constraints),ncol=nconstraints.cmf)
    boxmat<-diag(nclasses.cmf)
    boxmat<-cbind(boxmat,boxmat,boxmat)
    tol<-0.00004 #tolerance
    A<-matrix(1,1,nclasses) # full investment with tol
    A<-rbind(A, # Full investment
             c(rep(1,nclasses.cmf),rep(0,nclasses.cmf),rep(0,nclasses.cmf)), # allocation to taxable
             c(rep(0,nclasses.cmf),rep(1,nclasses.cmf),rep(0,nclasses.cmf)), # allocation to deferred
             c(rep(0,nclasses.cmf),rep(0,nclasses.cmf),rep(1,nclasses.cmf)), # allocation to exempt
             boxmat, # box min
             -boxmat, # box max
             t(rbind(cmf.cons.mat,cmf.cons.mat,cmf.cons.mat))) # user defined constraints


    rhs <- c(fullinvMin=1, # full investment >= 1-tol
             account.values[1]/sum(account.values)-tol, # allocation to taxable =
             account.values[2]/sum(account.values)-tol, # allocation to deferred =
             account.values[3]/sum(account.values)-tol, # allocation to exempt =
             cma.ta$boxMin, # box min >=
             -cma.ta$boxMax, # box max >=
             rep(0,length(cma.ta$constraints))) # user defined constraints >=
    const.dir=c("=",rep(">=",length(rhs)-1))
    out<-list()
    out$min<-solveLP(objVec,rhs,A,maximum=FALSE,const.dir = const.dir,lpSolve = TRUE)
    out$max<-solveLP(objVec,rhs,A,maximum=TRUE,const.dir = const.dir,lpSolve=TRUE)
    return(out)
}

#' Find the portfolio with the minimum standard deviation given a target return
#' and a set of capital market assumptions
#'
#' This function is used to create an efficient frontier which iterates across a
#' set of target returns. Requires quadprog
#'
#' @param target.ret The target return.  Should be a feasible return.
#' @param cma.ta Tax-aware capital market assumptions
#' @return The optimum produced by solve.QP from quadprog. The solution value is the set of weights.
#' @export
#' @seealso \code{\link[quadprog]{solve.QP}}
#' @keywords asset allocation efficient frontier target return
#'
optimize.target.return<-function(target.ret,cma.ta){
    require(quadprog)
    nclasses.cmf<-cma.ta$base.nclasses
    nclasses<-cma.ta$nclasses
    class.names <- cma.ta$classes
    ret<-cma.ta$ret
    cov.pd<-cma.ta$cov
    account.values<-cma.ta$account.values
    nconstraints.cmf<-length(cma.ta$constraints)
    #nconstraints<-nconstraints.cmf+3
    cmf.cons.mat<-matrix(unlist(cma.ta$constraints),ncol=nconstraints.cmf)

    boxmat<-diag(nclasses.cmf)
    boxmat<-cbind(boxmat,boxmat,boxmat)
    tol<-0.00004 #tolerance
    A<-matrix(ret,1,nclasses) #target return
    A<-rbind(A,matrix(1,1,nclasses)) # full investment with tol
    A<-rbind(A,matrix(-1,1,nclasses)) # full investment with tol
    A<-rbind(A, # Full investment
             c(rep(1,nclasses.cmf),rep(0,nclasses.cmf),rep(0,nclasses.cmf)), # allocation to taxable
             c(rep(0,nclasses.cmf),rep(1,nclasses.cmf),rep(0,nclasses.cmf)), # allocation to deferred
             c(rep(0,nclasses.cmf),rep(0,nclasses.cmf),rep(1,nclasses.cmf)), # allocation to exempt
             c(rep(-1,nclasses.cmf),rep(0,nclasses.cmf),rep(0,nclasses.cmf)), # allocation to taxable
             c(rep(0,nclasses.cmf),rep(-1,nclasses.cmf),rep(0,nclasses.cmf)), # allocation to deferred
             c(rep(0,nclasses.cmf),rep(0,nclasses.cmf),rep(-1,nclasses.cmf)), # allocation to exempt
             boxmat, # box min
             -boxmat, # box max
             diag(1,nclasses), # non-negative
             t(rbind(cmf.cons.mat,cmf.cons.mat,cmf.cons.mat))) # user defined constraints

    f <- c(targetret=target.ret, #target ret >=
           fullinvMin=1-tol, # full investment >= 1-tol
           fullinvMax=-(1+tol), # -full investment >= -(1+tol)
           account.values[1]/sum(account.values)-tol, # allocation to taxable =
           account.values[2]/sum(account.values)-tol, # allocation to deferred =
           account.values[3]/sum(account.values)-tol, # allocation to exempt =
           -(account.values[1]/sum(account.values)+tol), # allocation to taxable =
           -(account.values[2]/sum(account.values)+tol), # allocation to deferred =
           -(account.values[3]/sum(account.values)+tol), # allocation to exempt =
           cma.ta$boxMin-tol, # box min >=
           -(cma.ta$boxMax+tol), # box max >=
           rep(-tol,nclasses),
           rep(-tol,length(cma.ta$constraints))) # user defined constraints >=

    out <- try(solve.QP(Dmat=cov.pd, dvec = rep(0,nclasses), Amat=t(A), bvec=f, meq=0))
    if (class(out)=="try-error") out<-NULL
    return(out)
}

#' Use resampling to find an efficient portfolio
#'
#' This function finds an efficient frontier given a target standard deviation
#' and a set of capital market assumptions. Resampling reduces the impact of
#' estimation error in the mean-variance process.  The process starts with a set
#' of assumptions. These are used to generate a set of random returns (a
#' sample). The return and covariance of this sample is computed and an
#' efficient point is created. This sampling is repeated.  The average
#' allocations of the samples is the result. This function allows the user to
#' set a minimum threshold for an asset class.  If an asset class's weight fails
#' to meet the threshold, the class's maximum weight is set to zero.  The
#' resampling is repeated.  The threshold is ignored for asset classes with
#' minimum weights less than the threshold (for example, if one sets a 1%
#' minimum weight to cash and a threshold of 2.5%, then a 1% cash weighting will
#' be allowed.)
#'
#' @param target.risk The target risk for which to find an efficient portfolio.
#' @param cma.ta Tax-aware capital market assumptions
#' @param n.samples The number of samples for the resampling.
#' @param thresh The minimum weight for an asset class (if the assumptions do
#'   not specify another minimum)
#' @export
#' @return A list with two items.  The first item w is the weights of the
#'   efficient portfolio. The second item is mat which is the matrix of
#'   resampled solutions.  Each row is a sample. The first two columns are
#'   return and risk respectively.  The remainding columns are the weights.  The
#'   w item is the column means of the weights.
#'
resample.target.risk<-function(target.risk,cma.ta,n.samples=100,thresh=0){
    # Create the weights of a mean variant efficient portfolio with a target risk of target.risk
    # thresh is the minimum weight for a base asset class. If boxmin is non-zero for an asset class, then
    # the threshold is ignored (e.g if thresh is 0.025 but boxmin is 0.01, then the thresh is ignored
    require(MASS)
    require(xts)
    require(Matrix)
    tol<-0.00004
    # fun return the risk of a portfolio with with weights of x minus the target risk
    #fun <-function(x,cma.ta) portrisk(optimize.target.return(x,cma.ta)$solution,cma.ta)-target.risk
    fun <-function(x,cmf.x) portrisk(optimize.target.return(x,cmf.x)$solution,cmf.x)-target.risk
    resamp.mat<-matrix(0,nrow=n.samples,ncol=cma.ta$nclasses+2)
    colnames(resamp.mat)<-c("Return","Risk",cma.ta$classes)
    set.seed(101)
    stop.crit <-FALSE
    iteration<-1
    cmf.resamp<-cma.ta
    iter.uniroot<-1
    iter.opttarget<-1
    while (! stop.crit){
        for (i in 1:n.samples){
            result_type<-"try-error"
            while (result_type=="try-error"){
                R<-xts(mvrnorm(n=120,cma.ta$ret,cma.ta$cov,
                               empirical=FALSE),order.by=seq(as.Date("2000/12/31"),by="month",length.out=120))
                cmf.resamp$ret<-colMeans(R)
                cmf.resamp$cov<-nearPD(cov(R))$mat
                minmax<-find.minmax.return(cmf.resamp)
                targetret<-try(uniroot(fun,c(minmax$min$opt+.0005,minmax$max$opt),cmf.x=cmf.resamp)$root,silent = TRUE)
                result_type<-class(targetret)
                if (result_type=="try-error"){
                    print(paste("uniroot error",iter.uniroot))
                    iter.uniroot<-iter.uniroot+1
                } else {iter.uniroot<-1}
                if (result_type!="try-error"){
                    sol<-try(optimize.target.return(targetret,cmf.resamp)$solution,silent = TRUE)
                    result_type<-class(sol)
                    if (result_type=="try-error"){
                        print(paste("opttarget error",iter.opttarget))
                        iter.opttarget<-iter.opttarget+1
                    }
                }
            }
            resamp.mat[i,1]<-portret(sol,cmf.resamp)
            resamp.mat[i,2]<-portrisk(sol,cmf.resamp)
            resamp.mat[i,3:(cmf.resamp$nclasses+2)]<-sol
        }
        w<-colMeans(resamp.mat[,3:ncol(resamp.mat)]) #resampling wt is mean weight of each class across the samples
        w.ac<-rowSums(matrix(w,ncol=3)) # wts of base asset classes across account type
        idx <- cmf.resamp$boxMin==0 & w.ac<thresh & w.ac>tol
        print(paste("Iteration",iteration," sumidx",sum(idx)))
        # if all the asset classes with boxmin!=0 are above threshold then sum of idx will be zero
        stop.crit <- sum(idx)==0 | iteration>cmf.resamp$base.nclasses
        if (! stop.crit){
            iteration<-iteration+1
            # force asset class with least weight and boxmin==0 to 0 and rerun
            temp<-w.ac
            temp[cmf.resamp$boxMin>0]<-Inf
            temp[w.ac<=tol]<-Inf
            ac.tozero<-which.min(temp)
            cmf.resamp$boxMax[ac.tozero]<-0
            print(paste("boxmax",sum(cmf.resamp$boxMax>0)))
        }
    }
    out<-list()
    out$w<-colMeans(resamp.mat[,3:ncol(resamp.mat)])
    out$mat<-resamp.mat
    return(out)
}