#' Calculates slack 
#' 
#' Calculates weights that can be added or subtracted from a portfolios without violating box contraints.
#' Also returns the indices representing which asset classes may be increased and decreased
#'
#' @param x Portfolio weights
#' @param boxmin Box min constraints
#' @param boxmax Box max constraings
#' @param maxdelta Maximum amount by which to change an asset class
#'
#' @return
#'
#' @examples calc_slack(x, boxmin, boxmax, 0.03)
calc_slack <- function(x, boxmin, boxmax, maxdelta){
    incr <- pmin(pmax(0, boxmax - x), maxdelta)
    decr <- pmin(pmax(0, x - boxmin), maxdelta)
    classesToIncr <- which(incr>0)
    classesToDecr <- which(decr>0)
    return(list(incr=incr, decr=decr, classesToIncr=classesToIncr, classesToDecr=classesToDecr))
}

#' Portfolio optimization using a pairwise algorithm
#' 
#' This will attempt to maximize a supplied objective function within box constraints.
#' The algorithm begins by finding all the asset classes with weights that can be increased and those
#' that can be decreased.  Using pairs of classes within the same account, it increases one and decreases the other
#' finding the best pair to change.  If a pair improves the objective value, the process is repeated.  If no pair is found
#' the maxdelta is halved and the process is repeated until the delta is <= 0.00001.
#'
#' @param x Initial set of weights. Must sum to 1 (or whatever sum is desired).  Weights across accounts must be correct.
#' @param LB Box minimum
#' @param UB Box maximum
#' @param objFun Objective function
#' @param maxdelta Initial setting of delta. 
#' @param ... Parameters to be passed to the objective function
#'
#' @return List with MaxObj the value of the objective; x the corresponding portfolio weights, and Iter the number of iterations
#' @export
#'
#' @examples maximize_pairwise(x, LB, UB, objFun, maxdelta, ...)
maximize_pairwise <- function(x, LB, UB, objFun, maxdelta, ...){
    bestx <- x
    maxobj <- objFun(bestx, ...)
    slack <- calc_slack(bestx, LB, UB, maxdelta)
    bestfx <- maxobj
    
    iter <- 0
    while(maxdelta > 0.00001){
        iter <- iter + 1
        for(i in slack[["classesToIncr"]]){
            for(j in slack[["classesToDecr"]]){
                # only increase / decrease pairs within same account.
                if((i-1)%/%cma$nclasses == (j-1) %/% cma$nclasses & i != j){ # check for same account different class
                    delta <- min(slack[["incr"]][i], slack[["decr"]][j])
                    x<-bestx
                    x[i] <- bestx[i] + delta
                    x[j] <- bestx[j] - delta
                    fx <- objFun(x, ...)
                    if(fx > bestfx){
                        bestfx <- fx
                        besti <- i
                        bestj <- j
                    }
                }
            }
        }
        if(bestfx > maxobj){
            delta <- min(slack[["incr"]][besti], slack[["decr"]][bestj])
            bestx[besti] <- bestx[besti] + delta
            bestx[bestj] <- bestx[bestj] - delta
            maxobj <- bestfx
            slack <- calc_slack(bestx, boxmin, boxmax, maxdelta)
            cat(iter, maxobj, "\n")
        } else {
            maxdelta <- maxdelta/2
            slack <- calc_slack(bestx, boxmin, boxmax, maxdelta)
        }
        
    }
    out <- list()
    out$MaxObj <- maxobj
    out$x <- bestx
    out$Iter <- iter
    return(out)
}

# test <- maximize_pairwise(wts.bench.ta, boxmin, boxmax, aa_objective, OptimizationParameters$MinNonZeroWt,
#                          cma.ta, cma, wts.bench.ta, wts.bench, OptimizationParameters,
#                          ClassGroupConstraints, pctassets, curx = NULL, verbose = FALSE)

#' Asset Allocation objective function - benchmark sensitive
#' 
#' An asset allocation objective function calculating a weighted (w*aftertax + (1-w)*pretax) excess return less penalties some of which 
#' are related to a benchmark (risk less than benchmark, tracking error). Penalties applied to: Non-zero weights below a threshold;
#' tracking error above a threshold, the number of classes with non-zero weights below a minimum or above a maximum, and turnover
#' above a threshold if the curx is supplied. 
#'
#' @param x vector of initial weights at which objective is calculated. Sum of 
#' @param cma.ta tax-aware cma
#' @param cma cma
#' @param wts.bench.ta weights of benchmark using cma.ta classes 
#' @param wts.bench weights of benchmark using cma classes
#' @param OptimizationParameters optimization parameters
#' @param ClassGroupConstraints class group contraints
#' @param pctassets a vector with 3 named elements, taxed.pct, deferred.pct, exempt.pct representing the percentage in each type of account 
#' @param curx vector of current weights used to calculate turnover
#' @param verbose FALSE (default) indicates only the objective value is returned; TRUE also returns penalties and wtd excess return
#'
#' @return list - wtdexcessreturn: weighted excess return (after-tax, pre-tax), penalties: value of penalties, objectivevalue: objective value
#' @seealso [create.optimization.parameters()] for creating relevant optimization parameters
#' @export 
#'
#' @examples aa_objective(x, cma.ta, cma, wts.bench.ta, wts.bench, OptimizationParameters, ClassGroupConstraints, pctassets, curx=NULL)
aa_objective <- function(x, cma.ta, cma, wts.bench.ta, wts.bench, OptimizationParameters, ClassGroupConstraints, pctassets, curx=NULL, verbose=FALSE) {
    x.ac <- TaxAwareAA::calc.ac.wts(x, cma.ta)
    riskslack <- TaxAwareAA::portrisk(x.ac, cma) - TaxAwareAA::portrisk(wts.bench, cma)
    TEslack <- TaxAwareAA::portrisk(x.ac - wts.bench, cma) - OptimizationParameters$MaxTrackingError # pretax tracking error
    ac.data.ncol <- ncol(cma$ac.data)
    nzwts <- sum(x.ac[cma$ac.data$Min==0] > 0) # number of non-zero wts for classes with no minimum. 
    minnzwt <- ifelse(nzwts == 0, 0,  min(x.ac[cma$ac.data$Min==0][x.ac[cma$ac.data$Min==0]>0])) # min of wts that are non-zero and have a nonzero minimum
    turnoverslack <- ifelse(is.null(curx), 0, OptimizationParameters$MaxTurnover - sum(abs(x - curx)) / 2)
    wtdexcessreturn <- (TaxAwareAA::portret(x, cma.ta) - TaxAwareAA::portret(wts.bench.ta, cma.ta)) * OptimizationParameters$WtAfterTax + 
        (TaxAwareAA::portret(x.ac, cma) - TaxAwareAA::portret(wts.bench, cma)) * (1 - OptimizationParameters$WtAfterTax)
    penalties <- sum((x.ac[x.ac>cma$ac.data$Max] - cma$ac.data$Max[x.ac>cma$ac.data$Max])^2) + # ac box max
        sum((x.ac[x.ac<cma$ac.data$Min] - cma$ac.data$Min[x.ac<cma$ac.data$Min])^2) + # ac box min
        ifelse(turnoverslack <=0, 0, turnoverslack^2) +  
        ifelse(riskslack <= 0, 0, riskslack^2) + 
        ifelse(TEslack <= 0, 0 , TEslack^2) 
    for(i in (ac.data.ncol - cma$nconstraints + 1):ac.data.ncol){ # user defined constraints
        udconstraint <- sum(x.ac * cma$ac.data[,i])
        penalties <-penalties + ifelse(udconstraint >= 0, 0, udconstraint^2)
    }
    ngroups <- length(ClassGroupConstraints)/2
    for(i in 1:ngroups){
        cnames <- unlist(ClassGroupConstraints[i])
        ctol <- unlist(ClassGroupConstraints[ngroups + i])
        portgroupalloc <- sum(x.ac[cnames])
        benchgroupmin <- sum(wts.bench[cnames]) + ctol[1]
        benchgroupmax <- sum(wts.bench[cnames]) + ctol[2]
        penalties <-penalties + 
            ifelse(portgroupalloc >= benchgroupmin, 0, (portgroupalloc - benchgroupmin)^2) +
            ifelse(portgroupalloc <= benchgroupmax, 0, (portgroupalloc - benchgroupmax)^2)    
    }
    penalties <- penalties + 
        ifelse(nzwts >= OptimizationParameters$MinBaseClasses, 0, (nzwts - OptimizationParameters$MinBaseClasses)^2) +
        ifelse(nzwts <= OptimizationParameters$MaxBaseClasses, 0, (nzwts - OptimizationParameters$MaxBaseClasses)^2) +
        ifelse(minnzwt >= OptimizationParameters$MinNonZeroWt, 0, (minnzwt - OptimizationParameters$MinNonZeroWt)^2) # +
    
    if(OptimizationParameters$MinAcctCash){  
        cashclass <- which(cma$classes == OptimizationParameters$CashName)
        cashwts <- c(x[cashclass], x[cma$nclasses + cashclass], x[2 * cma$nclasses + cashclass])
        cashpct <- cashwts / pctassets
        penalties <- penalties + sum(pmin(cashpct - cma$ac.data$Min[cashclass], 0)^2)
    }
    names(penalties) <- NULL
    if(verbose){
        return(list(
            wtdexcessreturn = 100 * wtdexcessreturn,
            penalties = penalties*OptimizationParameters$PenaltyFactor,
            objectivevalue = 100*wtdexcessreturn - penalties*OptimizationParameters$PenaltyFactor))
    } else {
        return(100*wtdexcessreturn - penalties*OptimizationParameters$PenaltyFactor)
    }
}

#' Objective for finding minimum risk portfolio
#' 
#' An asset allocation objective function calculating -100 times pretax risk less penalties.  The -100 factor is to turn a minimization
#' problem into a maximization one.
#' Penalties applied to: Non-zero weights below a threshold; risk above a risk.budget threshold; weights outside of box limits;
#' tracking error above a threshold, the number of classes with non-zero weights below a minimum or above a maximum, and turnover
#' above a threshold if the curx is supplied.  
#'
#' @param x vector of initial weights at which objective is calculated
#' @param cma.ta tax-aware cma
#' @param cma cma
#' @param OptimizationParameters  optimization parameters
#' @param verbose FALSE (default) indicates only the objective value is returned; TRUE also returns penalties and other information
#'
#' @return list - wtdreturn: weighted return (after-tax, pre-tax), penalties: value of penalties, objectivevalue: objective value
#' @seealso [create.optimization.parameters()] for creating relevant optimization parameters
#' @export
#'
#' @examples aa_objective_min_risk(x, cma.ta, cma, OptimizationParameters)
aa_objective_min_risk <- function(x, cma.ta, cma, OptimizationParameters, verbose=FALSE) {
    x.ac <- TaxAwareAA::calc.ac.wts(x, cma.ta)
    pretax.port.risk <- TaxAwareAA::portrisk(x.ac, cma) 
    ac.data.ncol <- ncol(cma$ac.data)
    nzwts <- sum(x.ac > 0) # number of non-zero wts
    minnzwt <- ifelse(sum(x.ac > 0) == 0, 0,  min(x.ac[x.ac > 0]))
    penalties <- sum((x.ac[x.ac>cma$ac.data$Max] - cma$ac.data$Max[x.ac>cma$ac.data$Max])^2) + # ac box max
        sum((x.ac[x.ac<cma$ac.data$Min] - cma$ac.data$Min[x.ac<cma$ac.data$Min])^2) # ac box min
    for(i in (ac.data.ncol - cma$nconstraints + 1):ac.data.ncol){ # user defined constraints
        udconstraint <- sum(x.ac * cma$ac.data[,i])
        penalties <-penalties + ifelse(udconstraint >= 0, 0, udconstraint^2)
    }
    penalties <- penalties + 
        ifelse(nzwts >= OptimizationParameters$MinBaseClasses, 0, (nzwts - OptimizationParameters$MinBaseClasses)^2) +
        ifelse(nzwts <= OptimizationParameters$MaxBaseClasses, 0, (nzwts - OptimizationParameters$MaxBaseClasses)^2) +
        ifelse(minnzwt >= OptimizationParameters$MinNonZeroWt, 0, (minnzwt - OptimizationParameters$MinNonZeroWt)^2)
    names(penalties) <- NULL
    if(verbose){
        return(list(
            risk = 100 * pretax.port.risk,
            penalties = penalties*OptimizationParameters$PenaltyFactor,
            objectivevalue = -100*pretax.port.risk - penalties*OptimizationParameters$PenaltyFactor))
    } else {
        return(-100*pretax.port.risk - penalties*OptimizationParameters$PenaltyFactor)
    }
}

#' Asset Allocation objective function - benchmark insensitive
#' 
#' An asset allocation objective function calculating a weighted (w*aftertax + (1-w)*pretax) return less penalties.
#' Penalties applied to: Non-zero weights below a threshold; risk above a risk.budget threshold; weights outside of box limits;
#' tracking error above a threshold, the number of classes with non-zero weights below a minimum or above a maximum, and turnover
#' above a threshold if the curx is supplied.  
#'
#' @param x vector of initial weights at which objective is calculated
#' @param cma.ta tax-aware cma
#' @param cma cma
#' @param risk.budget constraint on pre-tax risk (really a penalty)
#' @param OptimizationParameters  optimization parameters
#' @param verbose FALSE (default) indicates only the objective value is returned; TRUE also returns penalties and other values
#' @param pctassets vector with weights of taxed.pct, deferred.pct, exempt.pct.  
#'
#' @return list - wtdreturn: weighted return (after-tax, pre-tax), penalties: value of penalties, objectivevalue: objective value
#' @seealso [create.optimization.parameters()] for creating relevant optimization parameters
#' @export
#'
#' @examples aa_objective_abs_risk(x, cma.ta, cma, risk.budget, OptimizationParameters)
aa_objective_abs_risk <- function(x, cma.ta, cma, risk.budget, OptimizationParameters, verbose=FALSE) {
    x.ac <- TaxAwareAA::calc.ac.wts(x, cma.ta)
    # sumxm1sqr <- (round(sum(x),5) - 1)^2
    riskslack <- TaxAwareAA::portrisk(x.ac, cma) - risk.budget
    ac.data.ncol <- ncol(cma$ac.data)
    nzwts <- sum(x.ac > 0) # number of non-zero wts
    minnzwt <- ifelse(sum(x.ac > 0) == 0, 0,  min(x.ac[x.ac > 0]))
    wtdreturn <- TaxAwareAA::portret(x, cma.ta) * OptimizationParameters$WtAfterTax + 
        TaxAwareAA::portret(x.ac, cma) * (1 - OptimizationParameters$WtAfterTax)
    penalties <- sum((x.ac[x.ac>cma$ac.data$Max] - cma$ac.data$Max[x.ac>cma$ac.data$Max])^2) + # ac box max
        sum((x.ac[x.ac<cma$ac.data$Min] - cma$ac.data$Min[x.ac<cma$ac.data$Min])^2) + # ac box min
        ifelse(riskslack <= 0, 0, riskslack^2)
    for(i in (ac.data.ncol - cma$nconstraints + 1):ac.data.ncol){ # user defined constraints
        udconstraint <- sum(x.ac * cma$ac.data[,i])
        penalties <-penalties + ifelse(udconstraint >= 0, 0, udconstraint^2)
    }
    penalties <- penalties + 
        ifelse(nzwts >= OptimizationParameters$MinBaseClasses, 0, (nzwts - OptimizationParameters$MinBaseClasses)^2) +
        ifelse(nzwts <= OptimizationParameters$MaxBaseClasses, 0, (nzwts - OptimizationParameters$MaxBaseClasses)^2) +
        ifelse(minnzwt >= OptimizationParameters$MinNonZeroWt, 0, (minnzwt - OptimizationParameters$MinNonZeroWt)^2)
    names(penalties) <- NULL
    if(verbose){
        return(list(
            wtdreturn = 100 * wtdreturn,
            penalties = penalties*OptimizationParameters$PenaltyFactor,
            objectivevalue = 100*wtdreturn - penalties*OptimizationParameters$PenaltyFactor))
    } else {
        return(100*wtdreturn - penalties*OptimizationParameters$PenaltyFactor)
    }
}

#' Find the minimum risk portfolio
#'
#' @param maxdelta Maximum a weight may be changed in the pair wise algo
#' @param ... parameters to pass to the aa_objective_min_risk function
#'
#' @return Results of maximize_pairwise function
#' @export
#' @seealso [maximize_pairwise()] for more detail on return values.
#'
#' @examples find_min_risk_portfolio(0.03, cma.ta, cma, 0.03, OptimizationParameters, FALSE)
find_min_risk_portfolio <- function(maxdelta, ...){
    min.risk.class <- which.min(cma$ac.data$risk)
    initwts <- rep(0, cma.ta$nclasses)
    initwts[min.risk.class] <- pctassets[1]
    initwts[min.risk.class + cma$nclasses] <- pctassets[2]
    initwts[min.risk.class + cma$nclasses * 2] <- pctassets[3]
    boxmax <- pmin(c(cma.ta$boxMax,   # smaller of asset class max and weight of entire account
                     cma.ta$boxMax,
                     cma.ta$boxMax), 
                   c(rep(pctassets[1], cma$nclasses),
                     rep(pctassets[2], cma$nclasses),
                     rep(pctassets[3], cma$nclasses)))
    temp <- maximize_pairwise(x=initwts,
                 LB=rep(0, cma.ta$nclasses),
                 UB=boxmax,
                 objFun=aa_objective_min_risk,
                 maxdelta =maxdelta,
                 ...)
    return(temp)
}

# names(pctassets) <- c("taxed.pct", "deferred.pct", "exempt.pct")
# test <- find_min_risk_portfolio(OptimizationParameters$MinNonZeroWt, 
#                                 cma.ta=cma.ta, cma=cma, OptimizationParameters=OptimizationParameters)
# test$x.ac <- TaxAwareAA::calc.ac.wts(test$x, cma.ta)
# test$Risk <- TaxAwareAA::portrisk(test$x.ac, cma)

#' Find Max Return portfolios for a risk budget
#'
#' @param initwts Initial Portfolio weigths
#' @param maxdelta Amount by which weights may be changed in one iteration
#' @param ... Parameters to pass to maximize_pairwise function
#'
#' @return
#' @export
#'
#' @examples find_max_return_portfolio(initwts, 0.03, ...)
find_max_return_portfolio <- function(initwts, maxdelta, ...){
    if(missing(initwts)){
        max.return.class <- which.max(cma$ac.data$geom.ret)
        initwts <- rep(0, cma.ta$nclasses)
        initwts[max.return.class] <- pctassets[1]
        initwts[max.return.class + cma$nclasses] <- pctassets[2]
        initwts[max.return.class + cma$nclasses * 2] <- pctassets[3]
    }
    ### box constraints
    boxmax <- pmin(c(cma.ta$boxMax,   # smaller of asset class max and weight of entire account
                     cma.ta$boxMax,
                     cma.ta$boxMax), 
                   c(rep(pctassets[1], cma$nclasses),
                     rep(pctassets[2], cma$nclasses),
                     rep(pctassets[3], cma$nclasses)))
    temp <- maximize_pairwise(x=initwts,
                 LB=rep(0, cma.ta$nclasses),
                 UB=boxmax,
                 objFun=aa_objective_abs_risk,
                 maxdelta =maxdelta,
                 ...)
    return(temp)
}

# risk.budget <- 1000
# test <- find_max_return_portfolio(maxdelta = OptimizationParameters$MinNonZeroWt,
#                                   cma.ta=cma.ta, cma=cma, risk.budget=risk.budget, OptimizationParameters=OptimizationParameters, verbose=FALSE)
# test$x.ac <- TaxAwareAA::calc.ac.wts(test$x, cma.ta)
# test$Risk <- TaxAwareAA::portrisk(test$x.ac, cma)
# test$WtdReturn <- TaxAwareAA::portret(test$x, cma.ta) * OptimizationParameters$WtAfterTax + 
#     TaxAwareAA::portret(test$x.ac, cma) * (1 - OptimizationParameters$WtAfterTax)


#' Create an efficient frontier using the pairwise iteration algorithm
#'
#' @param cma Capital Market Assumptions
#' @param cma.ta  Capital Market Assumptions - tax adjusted
#' @param OptimizationParameters  Optimization parameters
#' @param pctassets Percent of assets in each account
#' @param n.portfolios Number of points on the efficient frontier
#'
#' @return EFF object
#' @export
#'
#' @examples create.eff.pairwise(cma, cma.ta, OptimizationParameters, pctassets, 10)
create.eff.pairwise <- function(cma, cma.ta, OptimizationParameters, pctassets, n.portfolios=10){
    add_eff_info <- function(obj){
        names(obj$x)<-cma.ta$classes
        obj$x.ac <- TaxAwareAA::calc.ac.wts(obj$x, cma.ta)
        names(obj$x.ac)<- cma$classes
        obj$WtdReturn <- TaxAwareAA::portret(obj$x, cma.ta) * OptimizationParameters$WtAfterTax + 
            TaxAwareAA::portret(obj$x.ac, cma) * (1 - OptimizationParameters$WtAfterTax)
        obj$Risk <- TaxAwareAA::portrisk(obj$x.ac, cma)
        return(obj)
    }
    nclasses<-cma.ta$nclasses
    out<-matrix(0,nrow=n.portfolios,ncol=2+nclasses)
    colnames(out)<-c("Return","Risk",cma.ta$classes)
    minrisk <- find_min_risk_portfolio(OptimizationParameters$MinNonZeroWt,
                                       cma.ta=cma.ta, cma=cma, OptimizationParameters=OptimizationParameters)
    minrisk <- add_eff_info(minrisk)
    risk.budget <- 1000
    maxret <- find_max_return_portfolio(maxdelta = OptimizationParameters$MinNonZeroWt,
                                      cma.ta=cma.ta, cma=cma, risk.budget=risk.budget, OptimizationParameters=OptimizationParameters, verbose= FALSE)
    maxret <- add_eff_info(maxret)
    
    out[1,1] <- minrisk$WtdReturn
    out[1,2] <- minrisk$Risk
    out[1,3:(2+nclasses)] <- minrisk$x
    out[n.portfolios,1] <- maxret$WtdReturn
    out[n.portfolios,2] <- maxret$Risk
    out[n.portfolios,3:(2+nclasses)] <- maxret$x
    target.risk.vector <- seq(minrisk$Risk, maxret$Risk, length.out = n.portfolios)
    for(i in 2:(n.portfolios-1)){
        # initwts <- out[i-1, 3:(2+nclasses)]
        #maxrisk <- target.risk.vector[i]
        
        effport <- find_max_return_portfolio(initwts = out[i-1, 3:(2+nclasses)], maxdelta = OptimizationParameters$MinNonZeroWt,
                                             cma.ta=cma.ta, cma=cma, risk.budget = target.risk.vector[i], OptimizationParameters=OptimizationParameters, verbose=FALSE)
        effport <- add_eff_info(effport)
        out[i,1] <- effport$WtdReturn
        out[i,2] <- effport$Risk
        out[i,3:(2+nclasses)] <- effport$x
    }
    rownames(out)<-paste0("EffPt",seq(1,nrow(out)))
    class(out)<-"eff"
    return(out)
}

# test <- create.eff.pairwise(cma, cma.ta, OptimizationParameters, pctassets)

#' Expands an abbreviated (short) benchmark into a longer definition
#'
#' The Control.xlsx file allows the specification of a benchmark using only the non-zero asset classes. This function 
#' takes that definition as input and creates an expanded definition including the classes with zero weights both
#' pretax and aftertax
#'
#' @param short.benchmark 
#' @param cma CMA object
#' @param cma.ta CMA.ta object
#' @param investor Investor object
#'
#' @return List with two items: wts.bench and wts.bnech.ta
#' @export
#'
#' @examples expand.benchmark(short.benchmark, cma, cma.ta, investor)
expand.benchmark <- function(short.benchmark, cma, cma.ta, investor){
    wts.bench<-rep(0,cma$nclasses)
    names(wts.bench)<-cma$classes
    for (i in 1:length(short.benchmark)){
        wts.bench[names(short.benchmark[i])]<-short.benchmark[i]
    }
    wts.bench.ta<-c(wts.bench*investor["taxed"],wts.bench*investor["deferred"],wts.bench*investor["exempt"]) /
        (investor["taxed"]+investor["deferred"]+investor["exempt"])
    names(wts.bench.ta)<-cma.ta$classes
    return(list(wts.bench = wts.bench, wts.bench.ta = wts.bench.ta))
}
