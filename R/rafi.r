#' Load RAFI data into a cma
#' 
#' This function reads a set of returns and correlations from CSV files 
#' downloaded from the RAFI website.  Since RAFI may change its file formats, 
#' this function may not work as expected in the future. This data takes the raw
#' RAFI estimates wihch are real geometric. 
#' 
#' @param rafi.data.loc Folder in which the CSV files are located
#' @param acnametable Name of csv file containing asset class details
#' @param inflation rate Default calls the function 
#'   \code{\link{Get10YrBEInflationRate}}
#' @keywords asset allocation efficient frontier
#' @export
#' @return A cma object which is a list containing the as_of_date of the
#'   assumptions, the nclasses (number of classes), ac.data which is a table in
#'   which each row represents and asset class and columns contain return, risk
#'   and tax information.  This table also includes box and custom constraints
#'   There is also corr the correlation matrix, and/or cov for the covariance
#'   matrix and nconstraints for the number of constraints.
rafi.cma<-function(rafi.data.loc,acnametable="acname_table.csv",
                   inflation.rate=Get10YrBEInflationRate()$rate){
    ac_names<-read.csv(file=paste0(rafi.data.loc,acnametable),stringsAsFactors = FALSE)
    first.constraint.col<-match("Max",colnames(ac_names))+1 # Constraints begin after Max Col
    row.names(ac_names)<-ac_names$rt_class_names
    rafi.data<-rafi.load(rafi.data.loc,acnametable)
    ac_names<-ac_names[rafi.data$ret$Asset.class,] #re order to match ret which should match corr
    geom.ret<-rafi.data$ret$Expected.Return../100 +inflation.rate - ac_names$Expense
    arith.ret<-geom.ret + (rafi.data$ret$Volatility../100)^2/2
    yield<-rafi.data$ret$Yield../100
    idx<-yield<=0
    yield[idx]<-yield[idx]+inflation.rate
    growth<-rafi.data$ret$Growth../100
    growth[!idx]<-rafi.data$ret$Growth..[!idx]/100+inflation.rate
    val.change<-geom.ret-yield-growth
    ac.data<-data.frame(ret=arith.ret,geom.ret=geom.ret,yld=yield,growth=growth,
                        valChg=val.change,risk=rafi.data$ret$Volatility../100)
    ac.data<-cbind(ac.data,ac_names[rafi.data$ret$Asset.class,c("IntOrd","IntTE","DivQual","DivOrd","Turnover","LTCG","STCG","ForeignTaxWithheld","Expense","Min","Max")])
    nconstraints<-ncol(ac_names)-first.constraint.col+1
    if (nconstraints>0) {
        ac.data<-cbind(ac.data,ac_names[rafi.data$ret$Asset.class,first.constraint.col:ncol(ac_names)])
    }
    out<-list()
    out$as_of_date<-rafi.data$as_of_date
    out$classes<-row.names(ac_names)
    out$nclasses<-rafi.data$nclasses
    out$ac.data<-ac.data
    out$corr<-as.matrix(rafi.data$corr)
    out$cov<- as.matrix((ac.data$risk %*% t(ac.data$risk)) * out$corr)
    #out$boxMin<-cma$ac.data[,"Min"]
    #names(out$boxMin)<-cma$classes
    #out$boxMax<-cma$ac.data[,"Max"]
    #names(out$boxMax)<-cma$classes
    out$nconstraints<-nconstraints
    class(out)<-"cma"
    return(out)
}

#' Load RAFI data
#'
#' This function reads a set of returns and correlations from CSV files
#' downloaded from the RAFI website.  Since RAFI may change its file formats,
#' this function may not work as expected in the future.
#'
#' @param rafi.data.loc folder in which the CSV files are located
#' @param acnametable Name of csv file containing asset class details
#' @keywords asset allocation efficient frontier
#' @return A list with four items: ret is the return data, corr is the correlation matrix and
#' as_of_date is the date of the assumptionsfrom the csv file, nclasses is the number of classes
#'   correlation data
#'   
rafi.load<-function(rafi.data.loc,acnametable="acname_table.csv"){
    # The order and text of the asset class names of the returns and correlations may not match.  We want the classes to be
    # in the same order and have corresponding names.
    # A csv file is used to lookup new names for the asset classes. It also contains other information
    ac_names<-read.csv(file=paste0(rafi.data.loc,acnametable),stringsAsFactors = FALSE)
    #first.constraint.col<-match("Max",colnames(ac_names))+1 # Constraints begin after Max Col
    rafi<-rafi.read.csv(rafi.data.loc)
    rafi$ret[rafi$ret=="-"]<-0 # converts hyphens to zeros
    idx<-is.na(rafi$ret[,"Expected.Return.."])
    rafi$ret<-rafi$ret[!idx,] #remove extraneous rows
    rafi$ret$Asset.class<-unlist(sapply(rafi$ret$Asset.class,acname_lookup,type="ret",ac_names=ac_names))
    row.names(rafi$ret)<-rafi$ret$Asset.class
    rafi$as_of_date<-rafi$ret$As.of.Date[1]
    rafi$nclasses<-nrow(rafi$ret)
    rafi$corr<-rafi$corr[1:rafi$nclasses,4:ncol(rafi$corr)]
    rownames(rafi$corr)<-sapply(colnames(rafi$corr),acname_lookup,type="corr",ac_names=ac_names)
    colnames(rafi$corr)<-rownames(rafi$corr)
    rafi$ret<-rafi$ret[rownames(rafi$corr),] # put in same order as correlations
    for (i in 3:ncol(rafi$ret)){
        rafi$ret[,i]<-as.numeric(rafi$ret[,i])
    }
    return(rafi)
}

#' Read RAFI csv files
#' 
#' This function reads a set of returns and correlations from CSV files 
#' downloaded from the RAFI website.  No processing is done. This function 
#' allows one to see the data in an unprocessed form.
#' 
#' @param rafi.data.loc folder in which the CSV files are located
#' @keywords asset allocation efficient frontier
#' @return value A list with two items: ret is the return data, corr is the 
#'   correlation data
#'   
rafi.read.csv<-function(rafi.data.loc){
    out<-list()
    out$ret<-read.csv(paste0(rafi.data.loc,"core_asset_class_expected_returns.csv"),stringsAsFactors = FALSE)
    out$corr<-read.csv(paste0(rafi.data.loc,"core_asset_class_correlations_forecasted.csv"),stringsAsFactors = FALSE)
    return(out)
}

#' Look up asset class name in a table
#' 
#' RAFI does not provide consistent names for their asset classes. That is the names in the
#' return CSV file do not match the names in the correlation CSV.  Plus we may not prefer their names.
#' This function is called to look up a RAFI name and return a preferred name.
#'
#' @param ac is the RAFI name of the asset class
#' @param type is ret if the name comes from the return CSV else we assume it comes from the corr CSV
#' @param ac_names is the table.  This is loaded in \code{\link{rafi.load}}
#' @return A character string with the name of the asset class
#' 
acname_lookup<-function(ac,type,ac_names){
    #type is ret or corr for rafi_corr_class_name or rafi_ret_class_name
    if (type=="ret"){
        i<-which(ac_names[,1]==ac)
    } else {
        i<-which(ac_names[,2]==ac)
    }
    return(ac_names[i,3])
}

#' Calculates the pre-tax return
#'
#' Simulates the investment value without taxes
#'
#' @param yld is initial yield in decimal (e.g. 0.03 for 3 percent)
#' @param growth is the annual growth rate of the income in decimal.
#' @param valchange is the annual percentage change in the valuation in decimal
#' @param horizon number of years to simulate
#' @return Pretax annual return in decimal
#' 
pretax_return<-function(yld, growth, valchange, horizon=10){
    price<-100
    div0<-yld*price
    div<-div0
    shares<-1
    for (i in 1:horizon){
        div<-div*(1+growth)
        if (div==0 | div0==0){
            price<-(valchange+1)^i*100
        } else {
            price<-(valchange+1)^i*100/div0*div
        }
        income<-div*shares
        shares<-shares+income/price
    }
    return((shares*price/100)^(1/horizon)-1)
}

#' Check RAFI data
#'
#' This function checks the 2 CSV files from RAFI.
#' It checks that the names of the assets classes are defined in the acnametable
#'
#' @param rafi.data.loc folder in which the CSV files are located
#' @param acnametable Name of csv file containing asset class details
#' @keywords asset allocation efficient frontier
#' @export
#' @return A list with errors or and empty list is no errors
#' 
rafi.data.check<-function(rafi.data.loc,acnametable){
    ac_names<-read.csv(file=paste0(rafi.data.loc,acnametable),stringsAsFactors = FALSE)
    rafi<-rafi.read.csv(rafi.data.loc)
    
    n_acnames<-nrow(ac_names)
    n_acret<-nrow(rafi$ret)
    n_accorr<-nrow(rafi$corr)
    
    
    err_list<-list()
    n_err<-0
    
    if(n_acnames!=n_acret){
        n_err<-n_err+1
        err_list[[n_err]]<-"# Assets in return file not equal to # in acname_table"
    }
    if(n_acnames!=n_accorr){
        n_err<-n_err+1
        err_list[[n_err]]<-"# Assets in correlation file not equal to # in acname_table"
    }
    
    for(i in 1:n_acret){
        if(length(which(ac_names[,1]==rafi$ret$Asset.class[i]))==0){
            n_err<-n_err+1
            err_list[[n_err]]<-paste("Unrecognized asset class in RAFI return file",rafi$ret$Asset.class[i])
        }
    }
    for(i in 1:n_accorr){
        if(length(which(ac_names[,1]==rafi$corr$Index[i]))==0){
            n_err<-n_err+1
            err_list[[n_err]]<-paste("Unrecognized asset class in RAFI corr file",rafi$corr$Index[i])
        }
    }
    return(err_list)
}
