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
    investor.values<-c(taxed=value.taxable,deferred=value.deferred,exempt=value.exempt)
    taxrates<-c(OrdInc=(taxrate.ordinc+taxrate.state+taxrate.surcharge)*(1-taxrate.state),
                LTCG=(taxrate.LTCG+taxrate.state+taxrate.surcharge)*(1-taxrate.state),
                STCG=(taxrate.STCG+taxrate.state+taxrate.surcharge)*(1-taxrate.state),
                QualDiv=(taxrate.qualdiv+taxrate.state+taxrate.surcharge)*(1-taxrate.state),
                taxRState=taxrate.state)
    investor<-c(investor.values,
                taxrates,horizon=horizon)
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
    av<-data.frame(Account=c("Taxable","Deferred","Exempt","Total"),
                   Value=c(x["taxed"],x["deferred"],x["exempt"],x["taxed"]+x["deferred"]+x["exempt"]))
    tax<-data.frame(Tax=unlist(strsplit("Ordinary Income, LT Cap Gain, ST Cap Gain, Qual Div, State",split = ",")),
                    Rate=c(x["OrdInc"],x["LTCG"],x["STCG"],x["QualDiv"],x["taxRState"]))
    rownames(tax)<-NULL
    av$Account<-format(av$Account,justify="left") 
    av$Value<-prettyNum(round(av$Value,0),big.mark = ",")
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
#'
#' @return Sum of taxable, deferred and exempt values
#' @export
#'
investor.value<-function(investor){
    return(investor["taxed"]+investor["deferred"]+investor["exempt"])
}