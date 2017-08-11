#' Load RAFI data into a cma
#'
#' This function reads a set of returns and correlations
#' downloaded from the RAFI website. Combined with assumptions about
#' the investor, a cma object is returned representing capital
#' market assumptions.  RAFI files are in CSV format
#'  through April 2017.  From May 2017 they use a XLSX format.
#' 
#' @param version Version (either v1 or v2). Use v1 if RAFI files are in CSV format
#'  through April 2017.  From May 2017 they use a XLSX format.
#' @param rafi.data.loc Folder in which the RAFI input files are located
#' @param acnametable Name of csv or xlsx file containing asset class details
#' @param xls.file.name Name of XLSX RAFI files
#' @param file.ret CSV file name with return assumptions
#' @param file.corr CSV file name with correlation assumptions
#' @param inflation.rate Default calls the function
#'   \code{\link{Get10YrBEInflationRate}}
#' @keywords asset allocation efficient frontier
#' @export
#' @return A cma object which is a list containing the as_of_date of the
#'   assumptions, the nclasses (number of classes), ac.data which is a table in
#'   which each row represents and asset class and columns contain return, risk
#'   and tax information.  This table also includes box and custom constraints
#'   There is also corr the correlation matrix, and/or cov for the covariance
#'   matrix and nconstraints for the number of constraints.
#'   
rafi.cma <- function(version="v2",rafi.data.loc,acnametable="acname_table.xlsx",
                     xls.file.name="Asset-Allocation-Interactive-Data.xlsx",
                     file.ret="core_asset_class_expected_returns.csv",
                     file.corr="core_asset_class_correlations_forecasted.csv",
                     inflation.rate=Get10YrBEInflationRate()$rate){
    library(readxl)
    return(switch(toupper(version),
           V1 = rafi.cma.v1(rafi.data.loc,acnametable,file.ret,file.corr,inflation.rate),
           V2 = rafi.cma.v2(rafi.data.loc,acnametable,xls.file.name),
           stop("Invalid version in call to rafi.cma function. Should be v1 or v2")))
}

#' Load RAFI data into a cma
#' 
#' This function reads a set of returns and correlations from CSV files 
#' downloaded from the RAFI website.  Since RAFI may change its file formats, 
#' this function may not work as expected in the future. This data takes the raw
#' RAFI estimates wihch are real geometric. 
#' 
#' @param rafi.data.loc Folder in which the CSV files are located
#' @param acnametable Name of csv file containing asset class details
#' @param file.ret CSV file name with return assumptions
#' @param file.corr CSV file name with correlation assumptions
#' @param inflation rate Default calls the function 
#'   \code{\link{Get10YrBEInflationRate}}
#' @keywords asset allocation efficient frontier
#' @return A cma object which is a list containing the as_of_date of the
#'   assumptions, the nclasses (number of classes), ac.data which is a table in
#'   which each row represents and asset class and columns contain return, risk
#'   and tax information.  This table also includes box and custom constraints
#'   There is also corr the correlation matrix, and/or cov for the covariance
#'   matrix and nconstraints for the number of constraints.
rafi.cma.v1<-function(rafi.data.loc,acnametable="acname_table.csv",file.ret="core_asset_class_expected_returns.csv",
                      file.corr="core_asset_class_correlations_forecasted.csv",
                      inflation.rate=Get10YrBEInflationRate()$rate){
    ac_names<-read.csv(file=paste0(rafi.data.loc,acnametable),stringsAsFactors = FALSE)
    first.constraint.col<-match("Max",colnames(ac_names))+1 # Constraints begin after Max Col
    row.names(ac_names)<-ac_names$rt_class_names
    rafi.data<-rafi.load.v1(rafi.data.loc,acnametable,file.ret,file.corr)
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
#' @param file.ret CSV file name with return assumptions
#' @param file.corr CSV file name with correlation assumptions
#' @keywords asset allocation efficient frontier
#' @return A list with four items: ret is the return data, corr is the correlation matrix and
#' as_of_date is the date of the assumptionsfrom the csv file, nclasses is the number of classes
#'   correlation data
#'   
rafi.load.v1<-function(rafi.data.loc,acnametable="acname_table.csv",file.ret="core_asset_class_expected_returns.csv",
                       file.corr="core_asset_class_correlations_forecasted.csv"){
    # The order and text of the asset class names of the returns and correlations may not match.  We want the classes to be
    # in the same order and have corresponding names.
    # A csv file is used to lookup new names for the asset classes. It also contains other information
    ac_names<-read.csv(file=paste0(rafi.data.loc,acnametable),stringsAsFactors = FALSE)
    #first.constraint.col<-match("Max",colnames(ac_names))+1 # Constraints begin after Max Col
    rafi<-rafi.read.csv.v1(rafi.data.loc,file.ret,file.corr)
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
#' @param file.ret CSV file name with return assumptions
#' @param file.corr CSV file name with correlation assumptions
#' @keywords asset allocation efficient frontier
#' @export
#' @return value A list with two items: ret is the return data, corr is the 
#'   correlation data
#'   
rafi.read.csv.v1<-function(rafi.data.loc,file.ret="core_asset_class_expected_returns.csv",
                           file.corr="core_asset_class_correlations_forecasted.csv"){
    out<-list()
    out$ret<-read.csv(paste0(rafi.data.loc,file.ret),stringsAsFactors = FALSE)
    out$corr<-read.csv(paste0(rafi.data.loc,file.corr),stringsAsFactors = FALSE)
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
    rafi<-rafi.read.csv.v1(rafi.data.loc)
    
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

#' Load RAFI data into a cma
#' Version 2 uses RAFI file format which was adopted around May 2017
#' This new format includes a nominal return from which the inflation rate 
#' may be inferred.
#' 
#' This function reads a set of returns and correlations from XLSX files 
#' downloaded from the RAFI website.  Since RAFI may change its file formats, 
#' this function may not work as expected in the future.  
#' 
#' @param rafi.data.loc Folder in which the CSV files are located
#' @param acnametable Name of XLSX file containing asset class details
#' @param xls.file.name name of XLSX file with RAFI assumptions
#' @keywords asset allocation efficient frontier
#' @export
#' @return A cma object which is a list containing the as_of_date of the
#'   assumptions, the nclasses (number of classes), ac.data which is a table in
#'   which each row represents and asset class and columns contain return, risk
#'   and tax information.  This table also includes box and custom constraints
#'   There is also corr the correlation matrix, and/or cov for the covariance
#'   matrix and nconstraints for the number of constraints.
rafi.cma.v2<-function(rafi.data.loc,acnametable="acname_table.xlsx",xls.file.name="Asset-Allocation-Interactive-Data.xlsx"){
    rafi.data<-rafi.load.v2(rafi.data.loc,acnametable,xls.file.name)
    inflation.rate<-rafi.data$ret[1,"Expected Return (Nominal)"]-rafi.data$ret[1,"Expected Return (Real)"]
    ac_names<-ac_names<-data.frame(read_xlsx(paste0(rafi.data.loc,acnametable),sheet="acname_table"))
    row.names(ac_names)<-ac_names$rt_class_names
    first.constraint.col<-match("Max",colnames(ac_names))+1 # Constraints begin after Max Col
    ac_names<-ac_names[row.names(rafi.data$ret),] #re order to match ret which should match corr
    geom.ret<-rafi.data$ret$`Expected Return (Nominal)` - ac_names$Expense
    arith.ret<-geom.ret + rafi.data$ret$Volatility^2/2
    yield<-rafi.data$ret$`Average Net Yield`
    idx<-yield<=0
    yield[idx]<-yield[idx]+inflation.rate
    growth<-rafi.data$ret$`Capital Growth`
    growth[!idx]<-growth[!idx]+inflation.rate
    val.change<-geom.ret-yield-growth
    ac.data<-data.frame(ret=arith.ret,geom.ret=geom.ret,yld=yield,growth=growth,
                        valChg=val.change,risk=rafi.data$ret$Volatility)
    ac.data<-cbind(ac.data,ac_names[row.names(rafi.data$ret),c("IntOrd","IntTE","DivQual","DivOrd","Turnover","LTCG","STCG","ForeignTaxWithheld","Expense","Min","Max")])
    nconstraints<-ncol(ac_names)-first.constraint.col+1
    if (nconstraints>0) {
        ac.data<-cbind(ac.data,ac_names[row.names(rafi.data$ret),first.constraint.col:ncol(ac_names)])
    }
    
    idx <- ac_names$Max !=0
    out<-list()
    out$as_of_date<-rafi.data$as_of_date
    out$classes<-row.names(ac_names[idx,])
    out$nclasses<-sum(idx)
    out$ac.data<-ac.data[idx,]
    out$corr<-as.matrix(rafi.data$corr[idx,idx])
    out$cov<- as.matrix(rafi.data$cov[idx,idx])
    out$nconstraints<-nconstraints
    class(out)<-"cma"
    return(out)
}

#' Load RAFI data
#'
#' This function reads a set of returns and correlations from XLSX files
#' downloaded from the RAFI website.  Since RAFI may change its file formats,
#' this function may not work as expected in the future.
#'
#' @param rafi.data.loc folder in which the CSV files are located
#' @param acnametable Name of xlsx file containing asset class details
#' @param xls.file.name Name of Excel file downloaded from RAFI
#' @keywords asset allocation efficient frontier
#' @return A list with four items: ret is the return data, corr is the correlation matrix and
#' as_of_date is the date of the assumptionsfrom the csv file, nclasses is the number of classes
#'   correlation data
#'   
rafi.load.v2<-function(rafi.data.loc,acnametable="acname_table.xlsx",xls.file.name="Asset-Allocation-Interactive-Data.xlsx"){
    # The order and text of the asset class names of the returns and correlations may not match.  We want the classes to be
    # in the same order and have corresponding names.
    # A csv file is used to lookup new names for the asset classes. It also contains other information
    ac_names<-read_xlsx(paste0(rafi.data.loc,acnametable),sheet="acname_table")
    #first.constraint.col<-match("Max",colnames(ac_names))+1 # Constraints begin after Max Col
    rafi<-rafi.read.xls.v2(rafi.data.loc,xls.file.name="Asset-Allocation-Interactive-Data.xlsx")
    row.names(rafi$ret)<-unlist(sapply(rafi$ret$`Asset Class`,acname_lookup,type="ret",ac_names=ac_names))
    rafi$nclasses<-nrow(rafi$ret)
    rownames(rafi$corr)<-sapply(colnames(rafi$corr),acname_lookup,type="corr",ac_names=ac_names)
    colnames(rafi$corr)<-rownames(rafi$corr)
    rownames(rafi$cov)<-rownames(rafi$corr)
    colnames(rafi$cov)<-rownames(rafi$corr)
    rafi$ret<-rafi$ret[rownames(rafi$corr),] # put in same order as correlations
    return(rafi)
}

#' Read RAFI XLS file
#'  
#' This function reads a set of returns and correlations from XLS file 
#' downloaded from the RAFI website.  No processing is done. This function 
#' allows one to see the data in an unprocessed form.
#' 
#' @param rafi.data.loc folder in which the files are located
#' @param xls.file.name name of XLSX file
#' @keywords asset allocation efficient frontier
#' @export
#' @return value A list with two items: ret is the return data, corr is the 
#'   correlation data
#'   
rafi.read.xls.v2<-function(rafi.data.loc,xls.file.name="Asset-Allocation-Interactive-Data.xlsx"){
    if (file.exists(paste0(rafi.data.loc,"xlranges.yaml"))){
        xl<-yaml.load_file(paste0(rafi.data.loc,"xlranges.yaml"))
        rng.return<-xl$rng.return 
        rng.corr<-xl$rng.corr 
        rng.cov<- xl$rng.cov 
        rng.date<-xl$rng.date     
    } else {
        rng.return<-"B4:L32"
        rng.corr<-"C4:AD32"
        rng.cov<- "C35:AD63"
        rng.date<-"C50"
    }
    out<-list()
    out$as_of_date<-read_xlsx(paste0(rafi.data.loc,xls.file.name),sheet="Expected.Returns",range=rng.date)
    out$ret<-as.data.frame(read_xlsx(paste0(rafi.data.loc,xls.file.name),sheet="Expected.Returns",range=rng.return))
    rafi.corr<-read_xlsx(paste0(rafi.data.loc,xls.file.name),sheet="Expected.Risk.Matrix",range=rng.corr)
    row.names(rafi.corr)<-names(rafi.corr)
    rafi.cov<-read_xlsx(paste0(rafi.data.loc,xls.file.name),sheet="Expected.Risk.Matrix",range=rng.cov)
    row.names(rafi.cov)<-names(rafi.cov)
    out$corr<-rafi.corr
    out$cov<-rafi.cov
    return(out)
}
