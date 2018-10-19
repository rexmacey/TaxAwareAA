#' Create Investor
#' 
#' This function creates an investor object.
#' 
#' @param value.taxable Value in taxable account
#' @param value.deferred Value in deferred account
#' @param value.exempt Value in exempt account.
#' @param horizon Time horizon in years. Default is 10.
#' @param taxrate.state State income tax rate. Default is 0.
#' @param taxrate.ordinc Federal tax rate on ordinary income. Default is 0.
#' @param taxrate.LTCG Federal tax rate on long-term capital gains. Default is 
#'   0.
#' @param taxrate.STCG Federal tax rate on short-term capital gains. Default is 
#'   0.
#' @param taxrate.qualdiv Federal tax rate qualified dividends. Default is 0.
#' @param taxrate.surcharge Net investment income tax rate (section 1411 of 
#'   IRC). Default is 0.038.
#' @param income Income level. Might be used for looking up tax rates. Default 
#'   is NULL
#' @keywords asset allocation efficient frontier
#' @export
#' @return An investor class object which is a list containing items related to
#'   the investor. This package internally calculate income and capital gain
#'   rates that combine federal, state and surcharge taxes.
#'   
investor.create<-function(value.taxable,value.deferred,value.exempt,
                          horizon=10,
                          taxrate.state=0,
                          taxrate.ordinc=0,
                          taxrate.LTCG=0,
                          taxrate.STCG=0,
                          taxrate.qualdiv=0,
                          taxrate.surcharge=0.038,
                          income=NULL){
    investor.values<-c(taxed = value.taxable, 
                       deferred = value.deferred, 
                       exempt = value.exempt,
                       totalassets = value.taxable + value.deferred + value.exempt)
    pretax.pctwts <- investor.values / investor.values["totalassets"]
    names(pretax.pctwts) <- c("taxed.pct", "deferred.pct", "exempt.pct", "totalassets.pct")
    taxrates<-c(OrdInc=(taxrate.ordinc+taxrate.state+taxrate.surcharge)*(1-taxrate.state),
                LTCG=(taxrate.LTCG+taxrate.state+taxrate.surcharge)*(1-taxrate.state),
                STCG=(taxrate.STCG+taxrate.state+taxrate.surcharge)*(1-taxrate.state),
                QualDiv=(taxrate.qualdiv+taxrate.state+taxrate.surcharge)*(1-taxrate.state),
                taxRState=taxrate.state)
    temp <- value.deferred*(1-taxrates["OrdInc"])
    names(temp)<-""
    investor.values.at <- c(taxed.at = value.taxable,
                            deferred.at = temp,
                            exempt.at = value.exempt,
                            totalassets.at = value.taxable + temp + value.exempt) 
    investor<-c(investor.values,
                investor.values.at,
                pretax.pctwts,
                taxrates,
                horizon=horizon)
    class(investor)<-"investor"
    return(investor)
}

#' Print investor
#'
#' @param x Investor
#' @param ... Additional print parameters
#'
#' @return Prints
#' @export
#'
#'
print.investor<-function(x, ...){
    av<-data.frame(Account=c("Taxable", "Deferred", "Exempt", "Total"),
                   "PreTaxValue"=c(x["taxed"], x["deferred"], x["exempt"], x["totalassets"]),
                   "AfterTaxValue"=c(x["taxed.at"], x["deferred.at"], x["exempt.at"], x["totalassets.at"]),
                   "PreTaxPct"=c(x["taxed.pct"], x["deferred.pct"], x["exempt.pct"], x["totalassets.pct"]))
    av$PreTaxPct<-round(100*av$PreTaxPct,2)
    tax<-data.frame(Tax=unlist(strsplit("Ordinary Income, LT Cap Gain, ST Cap Gain, Qual Div, State",split = ",")),
                    Rate=c(x["OrdInc"],x["LTCG"],x["STCG"],x["QualDiv"],x["taxRState"]))
    rownames(tax)<-NULL
    av$Account<-format(av$Account,justify="left") 
    av$PreTaxValue<-prettyNum(format(round(av$PreTaxValue,0), scientific = FALSE),big.mark = ",")
    av$AfterTaxValue<-prettyNum(format(round(av$AfterTaxValue,0), scientific = FALSE),big.mark = ",")
    tax$Tax<-format(tax$Tax,justify="left") 
    tax$Rate<-prettyNum(tax$Rate*100,format="f",digits=4,nsmall=2)
    print(av,row.names=FALSE)
    cat("\n")
    print(tax,row.names=FALSE)
    cat("\n")
    cat(paste("Time horizon is ",x["horizon"],"years."))
    cat("\n")
}

#' Initial value in investor accounts
#'
#' @param investor Investor object
#' @param tax "after" for after-tax values, otherwise before-tax
#' @return Sum of taxable, deferred and exempt values
#' @export
#'
investor.value<-function(investor,tax="after"){
    if (tax=="after"){
        return(investor["taxed.at"]+investor["deferred.at"]+investor["exempt.at"])
    } else {
        return(investor["taxed"]+investor["deferred"]+investor["exempt"])
    }
}

#' Create a vector of the percent of pretax assets
#'
#' @param x investor object
#'
#' @return vector of numeric
#' @export
#'
#' @examples invstor.pctassets(investor)
investor.pctassets <- function(x){
   out <- c(x["taxed.pct"], 
            x["deferred.pct"], 
            x["exempt.pct"])
   names(out) <- c("taxed", "deferred", "exempt")
   return(out)
}