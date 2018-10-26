#' Calculate penalties 
#' Calculates the penalties used in objective functions
#' If wts.bench is present, then the penalty will include evaluation of risk relative to the risk of the benchmark
#' and tracking error.
#' If risk.budget is a parameter then the penalty will include evaluation of risk over the value of risk budget.
#' If curx is a parameter then the penalty will include evaluation of turnover.
#'
#' @param ... These parameters must include x - wts; cma.ta, cma, OptimizationParameters, verbose.
#'
#' @return if verbose, a vector of penalties including the total, otherwise just the scalar total.
#' @export
#'
#' @examples
calc_penalties <- function(...) {
    ddd <- list(...)
    if(! "x" %in% names(ddd)) stop("x (wts) missing from calc_penalties.")
    if(! "cma.ta" %in% names(ddd)) stop("cma.ta missing from calc_penalties.")
    if(! "cma" %in% names(ddd)) stop("cma missing from calc_penalties.")
    if(! "OptimizationParameters" %in% names(ddd)) stop("OptimizationParameters missing from calc_penalties.")
    if(! "verbose" %in% names(ddd)) verbose <- FALSE
    out <- numeric(12)
    names(out) <- unlist(strsplit("Risk,TrackingError,NonZeroWts,MinNonZeroWt,Turnover,BoxMin,BoxMax,UDConstraint,ClassMin,ClassMax,CashMin,Total", ","))
    x.ac <- TaxAwareAA::calc.ac.wts(ddd$x, ddd$cma.ta)
    if("wts.bench" %in% names(ddd)){
        riskslack <- TaxAwareAA::portrisk(x.ac, ddd$cma) - TaxAwareAA::portrisk(ddd$wts.bench, ddd$cma)
        out["Risk"] <- ifelse(riskslack <= 0, 0, riskslack^2)
        TEslack <- TaxAwareAA::portrisk(x.ac - ddd$wts.bench, ddd$cma) - ddd$OptimizationParameters$MaxTrackingError # pretax tracking error
        out["TrackingError"] <- ifelse(TEslack <= 0, 0 , TEslack^2)
    }
    if("risk.budget" %in% names(ddd)){
        riskslack <- TaxAwareAA::portrisk(x.ac, ddd$cma) - ddd$risk.budget
        out["Risk"] <- ifelse(riskslack <= 0, 0, riskslack^2)
    }
    if("ClassGroupConstraints" %in% names(ddd)){
        ngroups <- length(ClassGroupConstraints)/2
        for(i in 1:ngroups){
            cnames <- unlist(ddd$ClassGroupConstraints[i])
            ctol <- unlist(ddd$ClassGroupConstraints[ngroups + i])
            portgroupalloc <- sum(x.ac[cnames])
            benchgroupmin <- sum(ddd$wts.bench[cnames]) + ctol[1]
            benchgroupmax <- sum(ddd$wts.bench[cnames]) + ctol[2]
            out["ClassGroupConstraints"] <- ifelse(portgroupalloc >= benchgroupmin, 0, (portgroupalloc - benchgroupmin)^2) +
                                            ifelse(portgroupalloc <= benchgroupmax, 0, (portgroupalloc - benchgroupmax)^2)    
        }
    }
    nzwts <- sum(x.ac[ddd$cma$ac.data$Min==0] > 0) # number of non-zero wts for classes with no minimum. 
    out["NonZeroWts"] <-  ifelse(nzwts >= ddd$OptimizationParameters$MinBaseClasses, 0, (nzwts - ddd$OptimizationParameters$MinBaseClasses)^2) +
                          ifelse(nzwts <= ddd$OptimizationParameters$MaxBaseClasses, 0, (nzwts - ddd$OptimizationParameters$MaxBaseClasses)^2) 
    minnzwt <- ifelse(nzwts == 0, 0,  min(x.ac[ddd$cma$ac.data$Min==0][x.ac[ddd$cma$ac.data$Min==0]>0])) # min of wts that are non-zero and have a nonzero minimum
    out["MinNonZeroWt"]  <- ifelse(minnzwt >= OptimizationParameters$MinNonZeroWt, 0, (minnzwt - OptimizationParameters$MinNonZeroWt)^2)
    if("curx" %in% names(ddd)){
        turnoverslack <- ifelse(is.null(ddd$curx), 0, ddd$OptimizationParameters$MaxTurnover - sum(abs(ddd$x - ddd$curx)) / 2)    
        out["Turnover"] <- ifelse(turnoverslack <=0, 0, turnoverslack^2)
    }
    out["ClassMin"] <- sum((x.ac[x.ac<ddd$cma$ac.data$Min] - ddd$cma$ac.data$Min[x.ac<ddd$cma$ac.data$Min])^2) # ac box min
    out["ClassMax"] <- sum((x.ac[x.ac>ddd$cma$ac.data$Max] - ddd$cma$ac.data$Max[x.ac>ddd$cma$ac.data$Max])^2) # ac box max
    ac.data.ncol <- ncol(cma$ac.data)
    for(i in (ac.data.ncol - ddd$cma$nconstraints + 1):ac.data.ncol){ # user defined constraints
        udconstraint <- sum(x.ac * ddd$cma$ac.data[,i])
        out["UDConstraint"] <- out["UDConstraint"] + ifelse(udconstraint >= 0, 0, udconstraint^2)
        
    }
    if(OptimizationParameters$MinAcctCash & "pctassets" %in% names(ddd)){  
        cashclass <- which(ddd$cma$classes == ddd$OptimizationParameters$CashName)
        cashwts <- c(ddd$x[cashclass], ddd$x[ddd$cma$nclasses + cashclass], ddd$x[2 * ddd$cma$nclasses + cashclass])
        cashpct <- cashwts / ddd$pctassets[1:3]
        out["CashMin"] <- sum(pmin(cashpct - ddd$cma$ac.data$Min[cashclass], 0)^2)
    }
    out["Total"] <- sum(out) * ddd$OptimizationParameters$PenaltyFactor
    if(ddd$verbose){
        return(out)
    } else {
        return(out["Total"])
    }
}


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
    maxobj <- objFun(x=bestx, ...)
    slack <- calc_slack(x=bestx, boxmin=LB, boxmax=UB, maxdelta=maxdelta)
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
            slack <- calc_slack(bestx, LB, UB, maxdelta)
            # cat(iter, maxobj, "\n")
        } else {
            maxdelta <- maxdelta/2
            slack <- calc_slack(bestx, LB, UB, maxdelta)
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
aa_objective_rel_risk <- function(x, cma.ta, cma, wts.bench.ta, wts.bench, OptimizationParameters, ClassGroupConstraints, pctassets, curx=NULL, verbose=FALSE) {
    x.ac <- TaxAwareAA::calc.ac.wts(x, cma.ta)
    wtdexcessreturn <- (TaxAwareAA::portret(x, cma.ta) - TaxAwareAA::portret(wts.bench.ta, cma.ta)) * OptimizationParameters$WtAfterTax + 
        (TaxAwareAA::portret(x.ac, cma) - TaxAwareAA::portret(wts.bench, cma)) * (1 - OptimizationParameters$WtAfterTax)
    penalties <- calc_penalties(x=x, cma.ta=cma.ta, cma=cma, wts.bench.ta = wts.bench.ta, wts.bench = wts.bench, OptimizationParameters = OptimizationParameters, 
                                ClassGroupConstraints = ClassGroupConstraints, pctassets = pctassets, curx=curx,verbose = TRUE)
    if(verbose){
        return(list(
            wtdexcessreturn = 100 * wtdexcessreturn,
            penalties = penalties["Total"],
            objectivevalue = 100*wtdexcessreturn - penalties["Total"],
            penalty_detail <- penalties))
            
    } else {
        return(100*wtdexcessreturn - penalties["Total"])
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
    penalties <- calc_penalties(x=x, cma.ta=cma.ta, cma=cma, OptimizationParameters = OptimizationParameters, verbose = TRUE)
    if(verbose){
        return(list(
            risk = 100 * pretax.port.risk,
            penalties = penalties["Total"],
            objectivevalue = -100*pretax.port.risk - penalties["Total"],
            penalty_detail <- penalties))
    } else {
        return(-100*pretax.port.risk - penalties["Total"])
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
    wtdreturn <- TaxAwareAA::portret(x, cma.ta) * OptimizationParameters$WtAfterTax + 
        TaxAwareAA::portret(x.ac, cma) * (1 - OptimizationParameters$WtAfterTax)
    penalties <- calc_penalties(x=x, cma.ta=cma.ta, cma=cma, risk.budget=risk.budget, OptimizationParameters = OptimizationParameters, verbose = TRUE)
    if(verbose){
        return(list(
            wtdreturn = 100 * wtdreturn,
            penalties = penalties["Total"],
            objectivevalue = 100*wtdreturn - penalties["Total"],
            penalty_detail <- penalties))
    } else {
        return(100*wtdreturn - penalties["Total"])
    }
}

#' Find the minimum risk portfolio
#'
#' @param ... parameters to pass to the aa_objective_min_risk function
#'
#' @return Results of maximize_pairwise function
#' @export
#' @seealso [maximize_pairwise()] for more detail on return values.
#'
#' @examples find_min_risk_portfolio(0.03, cma.ta, cma, 0.03, OptimizationParameters, FALSE)
find_min_risk_portfolio <- function(...){
    min.risk.class <- which.min(cma$ac.data$risk)
    initwts <- rep(0, cma.ta$nclasses)
    initwts[min.risk.class] <- pctassets[1]
    initwts[min.risk.class + cma$nclasses] <- pctassets[2]
    initwts[min.risk.class + cma$nclasses * 2] <- pctassets[3]
    boxmax <- pmin(cma.ta$boxMax,   # smaller of asset class max and weight of entire account
                   c(rep(pctassets[1], cma$nclasses),
                     rep(pctassets[2], cma$nclasses),
                     rep(pctassets[3], cma$nclasses)))
    maxdelta <- c(0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.10)
   # maxdelta <- c(0.03, 0.04, 0.05)
    newfun <- function(md, ...){
        temp <- maximize_pairwise(x=initwts,
                                  LB=rep(0, cma.ta$nclasses),
                                  UB=boxmax,
                                  objFun=aa_objective_min_risk,
                                  maxdelta = md,
                                  ...)
    }
    temp <- lapply(maxdelta, newfun, ...)
    out <- temp[[which.max(sapply(temp, "[", "MaxObj"))]]
    out <- add_port_info(out, cma, cma.ta, OptimizationParameters)
    return(out)
}

# names(pctassets) <- c("taxed.pct", "deferred.pct", "exempt.pct")
# test <- find_min_risk_portfolio(OptimizationParameters$MinNonZeroWt, 
#                                 cma.ta=cma.ta, cma=cma, OptimizationParameters=OptimizationParameters)
# test$x.ac <- TaxAwareAA::calc.ac.wts(test$x, cma.ta)
# test$Risk <- TaxAwareAA::portrisk(test$x.ac, cma)

#' Find Max Return portfolios for a risk budget
#'
#' @param ... Parameters to pass to aa_objective_abs_risk except for x
#'
#' @return list of values
#' @export
#'
#' @examples 
#' find_max_return_portfolio(cma.ta=cma.ta, cma=cma, risk.budget=rb, OptimizationParameters=OptimizationParameters, verbose=FALSE)
find_max_return_portfolio <- function( ...){
    max.return.class <- which.max(cma$ac.data$geom.ret)
    initwts <- rep(0, cma.ta$nclasses)
    initwts[max.return.class] <- pctassets[1]
    initwts[max.return.class + cma$nclasses] <- pctassets[2]
    initwts[max.return.class + cma$nclasses * 2] <- pctassets[3]
    
    ### box constraints
    boxmax <- pmin(cma.ta$boxMax,   # smaller of asset class max and weight of entire account
                   c(rep(pctassets[1], cma$nclasses),
                     rep(pctassets[2], cma$nclasses),
                     rep(pctassets[3], cma$nclasses)))
    maxdelta <- c(0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.10)
  #  maxdelta <- c(0.03, 0.04, 0.05)
    newfun <- function(md, ...){
        if("risk.budget" %in% names(list(...))){
            objFunction <- aa_objective_abs_risk
        } else {
            objFunction <- aa_objective_rel_risk
        }
        temp <- maximize_pairwise(x=initwts,
                                  LB=rep(0, cma.ta$nclasses),
                                  UB=boxmax,
                                  objFun= objFunction,
                                  maxdelta = md,
                                  ...)
    }
    temp <- lapply(maxdelta, newfun, ...)
    out <- temp[[which.max(sapply(temp, "[", "MaxObj"))]]
    out <- add_port_info(out, cma, cma.ta, OptimizationParameters)
    return(out)
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
   
    nclasses<-cma.ta$nclasses
    out<-matrix(0,nrow=n.portfolios,ncol=2+nclasses)
    colnames(out)<-c("Return","Risk",cma.ta$classes)
    minrisk <- find_min_risk_portfolio(cma=cma, 
                                       cma.ta=cma.ta, OptimizationParameters=OptimizationParameters)
    minrisk <- add_port_info(minrisk, cma, cma.ta, OptimizationParameters)
    risk.budget <- 1000
    maxret <- find_max_return_portfolio(cma.ta=cma.ta, cma=cma, risk.budget=risk.budget, OptimizationParameters=OptimizationParameters, verbose= FALSE)
    maxret <- add_port_info(maxret, cma, cma.ta, OptimizationParameters)
    
    out[1,1] <- minrisk$wtdreturn
    out[1,2] <- minrisk$pretax.risk
    out[1,3:(2+nclasses)] <- minrisk$x
    out[n.portfolios,1] <- maxret$wtdreturn
    out[n.portfolios,2] <- maxret$pretax.risk
    out[n.portfolios,3:(2+nclasses)] <- maxret$x
    target.risk.vector <- seq(minrisk$pretax.risk, maxret$pretax.risk, length.out = n.portfolios)
    initwts <- c(rep(pctassets[1]/cma$nclasses, cma$nclasses),
                 rep(pctassets[2]/cma$nclasses, cma$nclasses),
                 rep(pctassets[3]/cma$nclasses, cma$nclasses))
    
    names(initwts) <- cma.ta$classes
    for(i in 2:(n.portfolios-1)){
        risk.budget <- target.risk.vector[i]
        effport <- find_max_return_portfolio(cma.ta=cma.ta, cma=cma, risk.budget = risk.budget, OptimizationParameters=OptimizationParameters, verbose=FALSE)
        
        #effport <- find_max_return_portfolio(initwts = initwts, maxdelta = OptimizationParameters$MinNonZeroWt,
        #                                     cma.ta=cma.ta, cma=cma, risk.budget = risk.budget, OptimizationParameters=OptimizationParameters, verbose=FALSE)
        
        # effport <- find_max_return_portfolio(initwts = out[i-1, 3:(2+nclasses)], maxdelta = OptimizationParameters$MinNonZeroWt,
        #                                     cma.ta=cma.ta, cma=cma, risk.budget = risk.budget, OptimizationParameters=OptimizationParameters, verbose=FALSE)
        effport <- add_port_info(effport, cma, cma.ta, OptimizationParameters)
        out[i,1] <- effport$wtdreturn
        out[i,2] <- effport$pretax.risk
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
#' @param benchmark.txt Benchmark in text format (e.g. "Munis=0.6,USLarge=0.4") 
#' @param cma CMA object
#' @param cma.ta CMA.ta object
#' @param investor Investor object
#'
#' @return List with two items: wts.bench and wts.bnech.ta
#' @export
#'
#' @examples expand.benchmark(short.benchmark, cma, cma.ta, investor)
create.benchmark <- function(benchmark.txt, cma, cma.ta, investor, OptimizationParameters){
    short.benchmark<-eval(parse(text=paste0("c(",benchmark.txt,")")))
    out <- list()
    out$x.ac <- rep(0,cma$nclasses)
    names(out$x.ac)<-cma$classes
    for (i in 1:length(short.benchmark)){
        out$x.ac[names(short.benchmark[i])]<-short.benchmark[i]
    }
    out$x<-c(out$x.ac * investor["taxed.pct"],
             out$x.ac * investor["deferred.pct"], 
             out$x.ac * investor["exempt.pct"])
    names(out$x)<-cma.ta$classes
    out <- add_port_info(out, cma, cma.ta, OptimizationParameters)
    return(out)
}

#' Adds portfolio information to a mix
#'
#' @param obj A list object containing x (obj$x) which are the aftertax weights
#' @param cma capital market assumptions
#' @param cma.ta capital market assumptions, tax-adjusted
#' @param OptimizationParameters optimization paramters
#'
#' @return The list with additional information including the pretax wts, return and risk stats
#' @export
#'
#' @examples add_port_info(ojb, cma, cma.ta, OptimizationParameters)
add_port_info <- function(obj, cma, cma.ta, OptimizationParameters){
    if(missing(cma.ta)){
        names(obj$x) <- cma$classes
        obj$x.ac <- obj$x
        obj$pretax.ret <- TaxAwareAA::portret(obj$x.ac, cma)
        obj$pretax.risk <- TaxAwareAA::portrisk(obj$x.ac, cma)
        obj$aftertax.ret <- NULL
        obj$aftertax.risk <- NULL
        obj$wtdreturn <- obj$pretax.ret
    } else {
        names(obj$x)<-cma.ta$classes
        obj$x.ac <- TaxAwareAA::calc.ac.wts(obj$x, cma.ta)
        names(obj$x.ac)<- cma$classes    
        obj$pretax.ret <- TaxAwareAA::portret(obj$x.ac, cma)
        obj$pretax.risk <- TaxAwareAA::portrisk(obj$x.ac, cma)
        obj$aftertax.ret <- TaxAwareAA::portret(obj$x, cma.ta)
        obj$aftertax.risk <- TaxAwareAA::portrisk(obj$x, cma.ta)
        obj$wtdreturn <- obj$aftertax.ret * OptimizationParameters$WtAfterTax + 
            obj$pretax.ret * (1 - OptimizationParameters$WtAfterTax)
    }
    return(obj)
}

#' Make allocation tables
#' Give a set of weights, the function returns two tables.  The weights in the allocationacrossaccount table
#' will sum to 100.  Each value represents the allocation of the entire portfolio.  The weights in the
#' allocationbyaccount shows the weight within each account.  The weights in each account will sum to 100.
#' These are useful for display. 
#' 
#' @param wts Weights across assets
#' @param cma.ta capital market assumptions, tax-advantaged
#'
#' @return list with two tables allocationacrossaccount and allocationbyaccount
#' @export
#'
#' @examples make_alloc_tables(wts, cma.ta)
make_alloc_tables<-function(wts, cma.ta){
    wts.df<-matrix(0,ncol=4,nrow=cma.ta$base.nclasses+1) #wts across accounts
    rownames(wts.df)<-c(cma.ta$base.classes,"Total")
    colnames(wts.df)<-c("Taxable","Deferred","Exempt","Total")    
    wts.df[1:cma.ta$base.nclasses,"Taxable"]<-wts[1:(cma.ta$base.nclasses)]
    wts.df[1:cma.ta$base.nclasses,"Deferred"]<-wts[(cma.ta$base.nclasses+1):(2*cma.ta$base.nclasses)]
    wts.df[1:cma.ta$base.nclasses,"Exempt"]<-wts[(2*cma.ta$base.nclasses+1):(3*cma.ta$base.nclasses)]    
    wts.df[wts.df<0]<-0 # remove negative wts
    wts.df<-wts.df/sum(wts.df) # adjust to sum to 1.0000 exactly
    wts.df[,"Total"]<-rowSums(wts.df)
    wts.df2<-round(100*prop.table(wts.df,2),2) # proportion within account
    wts.df["Total",]<-colSums(wts.df)
    wts.df<-round(100*wts.df,1)
    wts.df2["Total",]<-colSums(wts.df2)
    idx<-wts.df[,"Total"]>0.01 # remove rows with zero weights
    wts.df<-wts.df[idx,]
    idx<-wts.df["Total",]>0 # remove account types with no weight
    wts.df2<-wts.df2[,idx]
    idx <- wts.df2[,"Total"]>0
    wts.df2 <- wts.df2[idx,]
    return(list(allocationacrossaccount=wts.df, allocationbyaccount=wts.df2))
}
