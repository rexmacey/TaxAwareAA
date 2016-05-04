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
#' @return A list containing items related to the investor. This package
#'   internally calculate income and capital gain rates that combine federal,
#'   state and surcharge taxes.
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
    return(investor)
}
