#' Calculate after-tax expected returns
#'
#' An after-tax expected \emph{arithmetic} return is calculated for each asset
#' class assuming it were held in each of the three account types: taxable,
#' deferred and exempt. Arithmetic returns are more appropriate inputs into a
#' mean variance optimizer than geometric.
#'
#' @param cma Capital market assumption object
#' @param investor Investor object
#' @export
#' @return A list with three numeric vectors: at.return.taxable contains the
#'   after-tax expected returns for a taxable account, at.return.deferred for a
#'   deferred account, and at.return.exempt for an exempt account.
#'
ATReturn.calc<-function(cma,investor){
    out<-list()
    out$at.return.taxable<-apply(cma$ac.data,1,function(x) ATReturn.taxable(
        yld=x["yld"], growth=x["growth"], valChg=x["valChg"],
        intOrd=x["IntOrd"], intTE=x["IntTE"], divQual=x["DivQual"],
        divOrd=x["DivOrd"],
        turnover=x["Turnover"], LTCG=x["LTCG"], STCG=x["STCG"],
        taxROrdInc=investor["OrdInc"],
        taxRQDiv=investor["QualDiv"],
        taxRLTCG=investor["LTCG"],
        taxRSTCG=investor["STCG"],
        taxRState=investor["taxRState"],
        horizon=investor["horizon"]))
    out$at.return.deferred<-apply(cma$ac.data,1,function(x) ATReturn.deferred(
        yld=x["yld"], growth=x["growth"], valChg=x["valChg"],
        foreigntaxwithheld=x["foreigntaxwithheld"],
        taxROrdInc=investor["OrdInc"],
        taxRState=investor["taxRState"],
        risk=x["risk"],
        horizon=investor["horizon"]))
    out$at.return.exempt<-apply(cma$ac.data,1,function(x) ATReturn.exempt(
        yld=x["yld"], growth=x["growth"], valChg=x["valChg"],
        horizon=investor["horizon"]))
    names(out$at.return.exempt)<-names(out$at.return.deferred)
    return(out)
}

#' Calculation of after-tax return in a taxable account
#'
#' The after-tax return calculation in the taxable account is tedious but not
#' complicated. For each year we calculate the income that would be received.
#' The income on an investment (as opposed to what may be received by an
#' investor) grows by a growth rate.  The income is taxed based on its character
#' (ordinary, qualified, etc) and the investor's tax rate.  Likewise there is
#' capital gains depending on the growth and the turnover.  We assume the
#' initial basis equals the initial value (a restriction that could be altered
#' in future version).  The basis is updated each period according to sales and
#' purchases.  Looping through all the periods of the horizon we arrive at a
#' final income. This income along with the valuation change determines the
#' final value of the asset.  This value is adjusted for a final sale assuming
#' any gain (loss) is long-term.
#'
#' @param yld Yield on the investment
#' @param growth Growth rate of the income paid on the investment
#' @param valChg Change in valuation of the investment
#' @param intOrd Percent of income taxed at the ordinary rate
#' @param intTE  Percent of income not taxed
#' @param divQual Percent of income taxed at the qualified dividend rate
#' @param divOrd Percent of income taxed at the ordinary dividend rate
#' @param turnover Percent of asset sold in the current year. Impacts capital gains.
#' @param LTCG Percent of capital gains taxed at long-term rate
#' @param STCG Percent of capital gains taxed at short-term rate
#' @param taxROrdInc Tax rate on ordinary income
#' @param taxRQDiv Tax rate on qualified dividends
#' @param taxRLTCG Tax rate on long-term capital gains
#' @param taxRSTCG Tax rate on short-term capital gains
#' @param taxRState State income tax rate
#' @param horizon Number of year in forecast horizon
#' @export
#' @return The after-tax return of an asset invested in taxable account.
#'
ATReturn.taxable<-function(yld,growth,valChg,intOrd,intTE,divQual,divOrd,turnover,LTCG,STCG,
                           taxROrdInc,taxRQDiv,taxRLTCG,taxRSTCG,taxRState,horizon=10){
    # Calculates the After-Tax return from before tax inputs and tax rates.  Assumes losses can be used and
    # assumes gains are realized and taxed at horizon at long-term rate.
    price<-100 # initialize value and basis and price to $100
    shares<-1
    v<-price*shares
    b<-v
    div<-price*yld # dividend for Year=0
    yield<-div/price
    for (i in 1:horizon){
        div<-div*(1+growth) # Dividends per share
        if (yld==0 | div==0) {
            price<-(valChg+1)^i*100
        } else{
            price<-(valChg+1)^i / yld*div # (1+growth)^t * (Pbeg/Divbeg)*Divend
        }
        income<-v*yield # previous year's yield * value
        unrealgl <- shares * price - v # previous shares * current price - previous value
        taxIncome <- income * (intOrd * taxROrdInc + divQual * taxRQDiv + divOrd * taxROrdInc) +
            income * (intOrd + divQual + divOrd) * taxRState
        valuesold <- (v + unrealgl) * turnover
        basissold <- b * turnover
        realgl <- valuesold - basissold
        taxCG <- realgl * (LTCG * taxRLTCG + STCG * taxROrdInc) + realgl * taxRState
        yield <- div / price
        shares <- shares + (income - taxIncome - taxCG) / price
        v <- price * shares
        b <- b - basissold + valuesold + income - taxIncome - taxCG
    }
    # Adjust value for final sale. Assume all long term
    realgl <- v - b
    taxCG <- realgl * (taxRLTCG + taxRState)
    v <- v - taxCG
    return(((v / 100) ^ (1 / horizon) - 1))
}

#' Calculation of after-tax return in a deferred account
#'
#' The after-tax returns in a deferred account are pretty simple.  An investment
#' will grow to $(1+pre_tax_geometric_return)^horizon$ and that value will be
#' subject to tax.  After tax the investor will have this amount less what was
#' paid in federal and state taxes. We convert this dollar amount into a return
#' by raising it to the (1/horizon) and subtracting 1. The after-tax geometric
#' return is then transformed into an arithmetic return.  Most investors think
#' of the value of their deferred account before taxes.  If someone has $100 in
#' an IRA, they think it's worth $100 which ignores the deferred tax liability.
#' By maintaining the fiction that the account today is worth $100, we
#' understate the after-tax return by dividing an after-tax terminal value with
#' a before-tax starting value.  Since the final dollars are correct the
#' allocation is correct which is our goal.
#'
#' @param arith.ret Pre-tax geometric return.  The geometric return is used because
#'   this is the rate at which the asset will grow over time.
#' @param taxROrdInc Tax rate on ordinary income
#' @param taxRState State income tax rate
#' @param horizon Number of year in forecast horizon
#' @param risk Standard deviation of asset class.
#' @export
#' @return The after-tax arithmetic return of an asset invested in taxable account.
#'

ATReturn.deferred<-function(yld,growth,valChg,foreigntaxwithheld,taxROrdInc,taxRState=0,risk,horizon=10){
    out<-ATReturn.exempt(yld=yld,
                         growth=growth,
                         valChg = valChg,
                         foreigntaxwithheld = foreigntaxwithheld,
                         horizon=horizon)
    out <- out - risk^2/2  # The geom return will be taxed, not the arith. 
    out<-((1+out)^horizon*(1-taxROrdInc-taxRState))^(1/horizon)-1 # reduce by taxes
    out<-out+risk^2/2 # convert back to arithmetic
}

ATReturn.exempt<-function(yld,growth,valChg,foreigntaxwithheld,horizon=10){
    price<-100 # initialize value and basis and price to $100
    shares<-1
    v<-price*shares
    div<-price*yld # dividend for Year=0
    yield<-yld
    for (i in 1:horizon){
        div<-div*(1+growth) # Dividends per share
        if (yld==0 | div==0) {
            price<-(valChg+1)^i*100
        } else{
            price<-(valChg+1)^i / yld*div # (1+growth)^t * (Pbeg/Divbeg)*Divend
        }
        income<-v*yield # previous year's yield * value
        taxIncome <- income * foreigntaxwithheld
        yield <- div / price
        shares <- shares + (income - taxIncome) / price
        v <- price * shares
    }
    return((v / 100) ^ (1 / horizon) - 1)
}

