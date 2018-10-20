#' Initialize a CMA (Capital Market Assumptions)
#'
#' Creates a CMA object.  Expected that add.class and add.constraint will be
#' called to populate it.
#'
#' @param as_of_date Date assumptions were created. Default NULL.
#' @keywords asset allocation efficient frontier.
#' @export
#' @return A cma object with a list of items.
#'
cma.initialize<-function(as_of_date=NULL){
    out<-list()
    out$as_of_date<-as_of_date
    out$nclasses<-0
    out$corr<-NULL
    out$cov<-NULL
    out$constraints<-list()
    out$constraint.names<-list()
    out$acdata<-data.frame(Name=character(),
                           Ret=numeric(),
                           Yield=numeric(),
                           Growth=numeric(),
                           Risk=numeric(),
                           Min=numeric(),
                           Max=numeric(),
                           IntOrd=numeric(),
                           IntTE=numeric(),
                           DivQual=numeric(),
                           DivOrd=numeric(),
                           Turnover=numeric(),
                           LTCG=numeric(),
                           STCG=numeric())
    return(out)
}

#' Create CMA (Capital Market Assumptions)
#'
#' This function creates a cma object.
#'
#' @param as_of_date Date assumptions were created. Default NULL.
#' @param classes List of names of asset classes. Default list()
#' @param ret Vector of returns, same length as classes. In decimal (e.g. 0.05
#'   for five percent). Default vector("numeric").
#' @param yield Vector of yield, same length as classes. In decimal (e.g. 0.05
#'   for five percent). Default vector("numeric").
#' @param growth Vector of growth, same length as classes. In decimal (e.g. 0.05
#'   for five percent). Default vector("numeric").
#' @param risk Vector of standard deviations, same length as classes. In decimal
#'   (e.g. 0.05 for five percent). Default vector("numeric").
#' @param corr Correlation matrix, same length as classes. In decimal (e.g. 0.05
#'   for five percent). Default NULL.
#' @param cov Covariance matrix, same length as classes. In decimal (e.g. 0.05
#'   for five percent). Default NULL. If null calculates the covariance matrix
#'   from the correlation matrix and standard deviations if both are provided.
#' @param boxMin Vector of minimum wts per asset class. Default is 0 if classes
#'   is not empty otherwise list()
#' @param boxMax Vector of maximum wts per asset class. Default is 1 if classes
#'   is not empty otherwise list()
#' @param constraints List of constraints.  Each constraint is a numeric vector
#'   with length equal to number of classes. The dot (scalar) product of this
#'   vector and the portfolio weights must be at least zero to satisfy the
#'   constraint. Default List().
#' @param  constraints.names List of the names of constraints.  Length should
#'   match length of constraints. Default List().
#' @keywords asset allocation efficient frontier.
#' @export
#' @return A list containing items related to the CMA including: as of date, classes, number of classes
#' ret, yield, growth, risk, correlations, covariance, box min, box max and constraints.
#'
cma.create<-function(as_of_date=NULL, classes=list(), ret=vector("numeric"), yield=vector("numeric"),
                     growth=vector("numeric"), risk=vector("numeric"), corr=NULL, cov=NULL,
                     boxMin=0,boxMax=1,constraints=list(),constraints.names=list()){
    out<-list()
    out$as_of_date<-as_of_date
    out$classes<-classes
    out$nclasses<-length(classes)
    out$ret<-ret
    out$yield<-yield
    out$growth<-growth
    out$risk<-risk
    out$corr<-corr
    if (is.null(cov) & !is.null(corr)) { # calc cov from corr and risk
        out$cov <- (risk %*% (t(risk))) * corr
    } else {
        out$cov <- cov
    }
    if(!matrixcalc::is.positive.semi.definite(out$cov)) {
        out$cov <- Matrix::nearPD(out$cov, corr=FALSE, keepDiag=TRUE, maxit=1000)$mat
    }
    out$cov<-as.matrix(out$cov)
    if (boxMin==0){
        out$boxMin<-rep(0,length(classes))
    } else {
        out$boxMin<-boxMin
    }
    if (boxMax==1){
        out$boxMax<-rep(1,length(classes))
    } else {
        out$boxMax<-boxMax
    }
    if (is.null(constraints)) {
        out$constraints<-list()
    } else {
        out$constraints<-constraints
    }
    if (!is.null(constraints.names)) out$constraint.names<-constraint.names
    return(out)
}

#' Add constraint to a CMA
#'
#' This function adds a contraint to a cma
#'
#' @param constraint a numeric vector with length equal to number of classes.
#'   The dot (scalar) product of this vector and the portfolio weights must be
#'   at least zero to satisfy the constraint.
#' @param constraint.name Name of constraint. Default NULL.
#' @param cma Capital market assumption object
#' @keywords asset allocation efficient frontier.
#' @export
#' @return A cma with the constraint added.
#'
cma.add.constraint<-function(constraint,constraint.name,cma){
    out<-cma
    out$constraints<-c(cma$constraints,list(constraint))
    if (!is.null(constraint.name)) out$constraint.names<-c(cma$constraint.names,constraint.name)
    out$
    return(out)
}

#' Add (or replace) the covariance matrix in a cma
#'
#' @param cma Capial market assumptions object
#' @param cov Covariance matrix
#' @keywords asset allocation efficient frontier.
#' @export
#' @return A cma with the supplied covarance matrix.
cma.set.cov<-function(cov,cma){
    out<-cma
    out$cov<-cov
    return(out)
}

#' Add (or replace) the correlation matrix in a cma
#'
#' @param cma Capial market assumptions object
#' @param corr Covariance matrix
#' @keywords asset allocation efficient frontier.
#' @export
#' @return A cma with the supplied correlation matrix.
cma.set.corr<-function(corr,cma){
    out<-cma
    out$corr<-corr
    return(out)
}

#' Calculates the covariance matrix of a cma from its correlation matrix and
#' risk vector
#'
#' @param cma Capial market assumptions object
#' @keywords asset allocation efficient frontier.
#' @export
#' @return A cma with a covariance matrix computed from its correlation matrix
#'   and risk vector
cma.calculate.cov<-function(cma){
    out<-cma
    out$cov <- (cma$acdata$risk %*% (t(cma$acdata$risk))) * cma$corr
    return(out)
}

#' Calculates the risk vector of a cma from its covariance matrix
#'
#' @param cma Capial market assumptions object
#' @keywords asset allocation efficient frontier.
#' @export
#' @return A cma with the risk vector computed from its covariance matrix.
cma.calculate.risk<-function(cma){
    out<-cma
    out$acdata$risk <- diag(cma$cov)^.5
    return(out)
}

#' Retrieves the 10 year breakeven inflation rate from the St. Louis Fed (FRED)
#'
#' See \url{https://research.stlouisfed.org/fred2/series/T10YIE} for more info
#' Uses the Quandl package to retrieve the data.
#' @param dt Character representing date for which the rate is to be retrieved. YYYY-MM-DD. Default is system dae.
#' @keywords asset allocation efficient frontier.
#' @export
#' @return A list with a cma with date and rate items. The date is the date the statistic was calculated.
#'
Get10YrBEInflationRate<-function(dt=format(Sys.Date(),"%Y-%m-01")){
     x<-Quandl::Quandl("FRED/T10YIE",start_date = "2015-01-01",end_date = dt)
    out<-list()
    out$date<-x[1,1]
    out$rate<-x[1,2]/100
    return(out)
}

#' Create a tax-aware CMA
#'
#' @param cma A cma object that is not tax-aware
#' @param investor An investor object
#' @export
#' @return A cma.ta object.
cma.ta.create<-function(cma,investor){
    class.names <- c(paste0(cma$classes,"-taxed"),
                     paste0(cma$classes,"-defer"),
                     paste0(cma$classes,"-exempt"))
    first.constraint.col<-match("Max",colnames(cma$ac.data))+1 # Constraints begin after Max Col
    out<-list()
    out$as_of_date<-cma$as_of_date
    out$classes<-class.names
    out$nclasses<-length(class.names)
    at.returns<-ATReturn.calc(cma,investor)
    out$ret<-c(at.returns$at.return.taxable,at.returns$at.return.deferred,at.returns$at.return.exempt)
    out$ret.geom<-c(at.returns$at.return.taxable.geom,at.returns$at.return.deferred.geom,at.returns$at.return.exempt.geom)
    names(out$ret)<-class.names
    risk.ta<-c(cma$ac.data$risk*(1-investor["LTCG"]),cma$ac.data$risk,cma$ac.data$risk) # tax adj risk (std dev)
    corr.ta<-cbind(cma$corr,cma$corr,cma$corr)
    corr.ta<-rbind(corr.ta,corr.ta,corr.ta)
    cov.ta<-risk.ta %*% t(risk.ta) * corr.ta # adjusted for taxes
    rownames(cov.ta)<-class.names
    colnames(cov.ta)<-class.names
    if(!matrixcalc::is.positive.semi.definite(cov.ta)) {
        cov.ta <- Matrix::nearPD(cov.ta, corr=FALSE, keepDiag=TRUE, maxit=1000)$mat
    }
    out$cov.ta<-as.matrix(cov.ta)
    out$boxMin <- rep(0, length(class.names))
    names(out$boxMin) <- class.names #cma$classes
    out$boxMax<-cma$ac.data$Max
    out$boxMax <- pmin(out$boxMax, c(rep(investor["taxed.pct"], cma$nclasses), 
                                 rep(investor["deferred.pct"], cma$nclasses), 
                                 rep(investor["exempt.pct"], cma$nclasses)))
    names(out$boxMax)<-class.names #cma$classes
    out$constraints<-list()
    for (i in 1:cma$nconstraints){
        out$constraints[[i]]<-cma$ac.data[,first.constraint.col+i-1]
        names(out$constraints[[i]])<-cma$classes
    }
    out$base.classes<-cma$classes
    out$base.nclasses<-cma$nclasses
    out$base.boxMin <- cma$ac.data$Min
    out$base.boxMax <- cma$ac.data$Max
    out$account.values<-c(investor["taxed"],investor["deferred"],investor["exempt"])   #investor$account.values
    out$inflation<-cma$inflation
    class(out)<-"cma.ta"
    return(out)
}

#' Print cma.ta object
#'
#' @param cma.ta cma.ta object
#' @param kable TRUE returns result of the knitr kable function.
#' @param ... Additional print parameters
#' @return object to print
#' @export
#'
print.cma.ta<-function(cma.ta, kable=TRUE, ...){
    nclasses.base<-cma.ta$base.nclasses
    cat("As of",cma.ta$as_of_date,"\n" )
    cat("Number of asset classes (per account type)",nclasses.base,"\n")
    temp<-data.frame(matrix(cma.ta$ret.geom*100,ncol=3))
    temp<-cbind(temp,100*diag(matrix(cma.ta$cov,nrow = cma.ta$nclasses))[1:nclasses.base]^.5)
    temp<-cbind(temp,cma.ta$base.boxMin*100,cma.ta$base.boxMax*100)
    rownames(temp)<-cma.ta$base.classes
    colnames(temp)<-c("Taxable Ret%","Deferred Ret%","Exempt Ret%","Risk%","MinWt","MaxWt")
    temp<-round(temp,2)
    if (kable){
        require(knitr)
        return(knitr::kable(temp,caption="Capital Market Assumptions"))
    } else {
        return(temp)
    }
}

#' Print cma object
#'
#' @param cma cma object
#'
#' @return NULL
#' @export
#'
print.cma<-function(cma){
    cat("As of",cma$as_of_date,"\n" )
    cat("Number of asset classes",cma$nclasses,"\n\n")
    #temp<-cma$ac.data[,c("LongName","AssetClass","geom.ret","risk")]
    temp<-cbind(cma$ac.data[,c("LongName","AssetClass","geom.ret","risk")],row.names(cma$ac.data))
    temp[,3:4]<-round(100*temp[,3:4],1)
    colnames(temp)<-c("Asset Class","Broad Class","Return%","Risk%","Abbrev")
    temp<-temp[order(temp$'Risk%',temp$'Asset Class'),]
    print(temp, right=FALSE, row.names=FALSE)
    cat("\nInflation ",round(100*cma$inflation,1),"% \n",sep="" )
    plot(cma$ac.data[,"risk"]*100,cma$ac.data[,"geom.ret"]*100,
         main="Asset Class Assumptions", xlab="Std Dev %", ylab="Return %",col="blue",pch=16)
    loc<-100*cma$ac.data[,c("risk","geom.ret")]
    loc[,2]<-loc[,2]-0.25 #move down a bit
    text(loc,cma$classes)
    abline(h=100*cma$inflation,col="red")
    invisible(NULL)
}
