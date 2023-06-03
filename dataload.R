runprocess = 1
while (runprocess == 1)
{
  library(ggplot2)    ## grammar of graphics
  library(reshape2)   ##reshaping data frames
  library(lattice)    ##More graphics
  library(hexbin)     ## ... and more graphics
  library(gridExtra)  ## even more graphics
  library(xtable)     ##Latex formatting of tables
  library(splines)    ##splines -- surprise :-)
  library(survival)   ##survival analysis
  library(grid)       ##for 'unit'
  library(lpSolve)    ##linear programming
  library(tidyverse)  ##
  library(tidyquant)  ## quant library 1
  library(quantmod)   ## quant library 2
  library(Quandl)     ## quant library 3
  library(fasttime)   ## time conversion
  library(xlsx)       ## omskrive til excel
  library(readxl)     ## read excel files
  library(readODS)    ## read ODS files
  library(xml2)       ## read xml files
  library(data.table)
  library(haven)
  library(dplyr)
  library(curl)
  library(jsonlite)
  library(plyr)
  runprocess = 0
}


color <- "##830E0D"
##########################################################################################################################################################
options(scipen=999)

#sadly at current this has to be added by hand.
stocks <- c("KO", "JPM");stocks = data.frame(symbol = stocks)

#can read, .csv, .txt, .xlsx, some website-data and stocks in above list with tq_get.
data = "C:\\Users\\Markb\\OneDrive\\Skrivebord\\Excel arks\\bogliste.xlsx" 

texcol = "no" #yes, no, true, false
NA_remove = "no" #yes, no, true, false

##########################################################################################################################################################
# data load process
runprocess = 1
while (runprocess == 1)
{
  #setup of basic stuff for running the process.
  #variables and functions
  from_d = '1980-01-01' #for downloading stockdata
  to_d   = '2022-12-01' #for downloading stockdata
  l <- list() #for finding the corresponding columns to matching names.
  substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))} #for extracting the last letters in a string
  substrLeft <- function (x, n) {substr(x,1,n)}
  #######################################################
  if(substrRight(data, 4) == paste("xlsx")) 
  {
    data2 <- read.xlsx(data, 1)
  }
  else if (substrRight(data,4) == paste("xlsm"))
  {
    data2 <- read.xlsx(data, 1)
  }
  else if (substrRight(data,3) == paste("xls"))
  {
    data2 <- read.xlsx(data, 1)
  }
  
  #######################################################
  else if (substrRight(data,3) == paste("ods"))
  {
    data2 <- read_ods(data, sheet = 1)
  }
  else if (substrRight(data,3) == paste("xml"))
  {
    data2 <- read_xml(data)
  }
  
  #######################################################
  else if (substrRight(data,3) == paste("csv"))
  {
    data2 <- read.table(data)
    if (unlist(gregexpr('\"',data2[1,]))[1]>0) {data2 <- as.data.frame(sapply(data2, function(x) gsub("\"", "", x)))}
    else if (unlist(gregexpr('\"',data2[1,]))[2]>0) {data2 <- as.data.frame(sapply(data2, function(x) gsub("\"", "", x)))}
    
    if ( unlist(gregexpr(';', data2[1,]))[1]>0 ) {data2 <- read.table(data, sep = ";", header=TRUE)}
    else if ( unlist(gregexpr(';', data2[1,]))[2]>0 ) {data2 <- read.table(data, sep = ";", header=TRUE)}
    else if ( unlist(gregexpr(',', data2[1,]))[1]>0 ) {data2 <- read.table(data, sep = ",", header=TRUE)}
    else if ( unlist(gregexpr(',', data2[1,]))[2]>0 ) {data2 <- read.table(data, sep = ",", header=TRUE)}
    else if ( unlist(gregexpr(' ', data2[1,]))[1]>0 ) {data2 <- read.table(data, sep = " ", header=TRUE)}
    else if ( unlist(gregexpr(' ', data2[1,]))[2]>0 ) {data2 <- read.table(data, sep = " ", header=TRUE)}
    else {data2 <- read.table(data)}
    if (colnames(data2)[1] == paste0("V",1) & colnames(data2)[2] == paste0("V",2)) 
    {
      data2 <- read.table(data, header = TRUE)
    }
  }
  else if (substrRight(data,3) == paste("txt"))
  {
    data2 <- read.table(data)
    if (unlist(gregexpr('\"',data2[1,]))[1]>0) {data2 <- as.data.frame(sapply(data2, function(x) gsub("\"", "", x)))}
    else if (unlist(gregexpr('\"',data2[1,]))[2]>0) {data2 <- as.data.frame(sapply(data2, function(x) gsub("\"", "", x)))}
    
    if ( unlist(gregexpr(';', data2[1,]))[1]>0 ) {data2 <- read.table(data, sep = ";", header=TRUE)}
    else if ( unlist(gregexpr(';', data2[1,]))[2]>0 ) {data2 <- read.table(data, sep = ";", header=TRUE)}
    else if ( unlist(gregexpr(',', data2[1,]))[1]>0 ) {data2 <- read.table(data, sep = ",", header=TRUE)}
    else if ( unlist(gregexpr(',', data2[1,]))[2]>0 ) {data2 <- read.table(data, sep = ",", header=TRUE)}
    else if ( unlist(gregexpr(' ', data2[1,]))[1]>0 ) {data2 <- read.table(data, sep = " ", header=TRUE)}
    else if ( unlist(gregexpr(' ', data2[1,]))[2]>0 ) {data2 <- read.table(data, sep = " ", header=TRUE)}
    else {data3 <- read.table(data)}
    if (colnames(data2)[1] == paste0("V",1) & colnames(data2)[2] == paste0("V",2))  
    {
      data2 <- read.table(data, header = TRUE)
    }
  }
  
  #######################################################
  else if (substrRight(data,3) == paste("dta"))
  {
    data2 <- read_stata(NULL)
  }
  else if (substrRight(data,8) == paste("sas7bdat"))
  {
    data2 <- read_sas(NULL, NULL)
  }
  
  #######################################################
  else if (substrLeft(data,4) == paste("www."))
  {
    data2 <- read.table(data) 
  }
  else if (is.null(
    for(x in 1:nrow(stocks))
    {
      if ( (unlist(gregexpr(stocks[x,1], data, TRUE))>0)  & (substrRight(data,2) == substrRight(stocks,2)) & (substrLeft(data,2) == substrLeft(stocks,2)) )
      {
        cat("This is stock number",print(x),"on the list")
      }
    }
  )
  ) 
  {
    data2 <- tq_get(data, get="stock.prices", from = from_d,to = to_d, period = "daily")
    # skip_to_next <- FALSE
    # tryCatch({
    #  a <- tq_get(x, get="dividends", from = from_d, to = to_d)
    #  write.xlsx(a, paste(path,x,".xlsx", sep=""), sheetName = "Dividend", append=FALSE)
    # },error = function(e) {skip_to_next = TRUE})
    # if(skip_to_next) {next}
    # b <- tq_get(x, get="stock.prices", from = from_d,to = to_d, period = "daily")            # daily stock prices
    # c <- tq_get(x, get="stock.prices",from = from_d, to = to_d)%>%
    #  tq_transmute(select = close, mutate_fun = to.period, period = "weeks")                   # weekly transform
    # d <- tq_get(x, get="stock.prices",from = from_d, to = to_d)%>%
    #  tq_transmute(select = close,mutate_fun = to.period, period = "months")                   # montly transform
    # write.xlsx(b, paste(path,x,".xlsx", sep=""), sheetName = "Daily", append=TRUE)      # write excel for dividends
    # write.xlsx(c, paste(path,x,".xlsx", sep=""), sheetName = "Weekly", append=TRUE)      # write excel for dividends
    # write.xlsx(d, paste(path,x,".xlsx", sep=""), sheetName = "Monthly", append=TRUE)     # write excel for dividends
    # yeardifs <- c((tail(d,1)[1,2]-tail(d,3)[1,2])/tail(d,3)[1,2], (tail(d,1)[1,2]-tail(d,12)[1,2])/tail(d,12)[1,2],(tail(d,1)[1,2]-tail(d,36)[1,2])/tail(d,36)[1,2],(tail(d,1)[1,2]-tail(d,60)[1,2])/tail(d,60)[1,2])
    # meanvals <- c(sum(tail(d,3)[1:3,2])/3,sum(tail(d,12)[1:12,2])/12,sum(tail(d,36)[1:36,2])/36,sum(tail(d,60)[1:60,2])/60)
    # volatilvals <- c((max(tail(d,3)[,2])-min(tail(d,3)[,2]))/meanvals[1],(max(tail(d,12)[,2])-min(tail(d,12)[,2]))/meanvals[2],(max(tail(d,36)[,2])-min(tail(d,36)[,2]))/meanvals[3],(max(tail(d,60)[,2])-min(tail(d,60)[,2]))/meanvals[4])
    # h <- cbind(yeardifs,meanvals,volatilvals)
    #rownames(h) <- c("3month","1year", "3year", "5year")
    #
    #write.xlsx(h, paste(path,x,".xlsx", sep=""), sheetName = "financials", append=True)
  }
  
  if (texcol == "yes" | texcol == "Yes" | texcol == "YES"| texcol == "1" | texcol == "y" | texcol == "true" | texcol == "True" | texcol == "TRUE")
  {
    data3 <- list()
    cat("you have decided to remove text columns\n")
    for (x in 1:ncol(data2))
    {
      if (is.character(data2[,x][[1]]))
      {
        cat("column",x,"is a column containing text\n")
      }
      else if (is.numeric(data2[,x][[1]]))
      {
        l[x] = x
        data3 <- append(data3, data2[,x][[1]])
        data <- data.frame(matrix(unlist(data3), nrow = nrow(data2), byrow = FALSE)) #aktie = (nrow = nrow(data2), byrow = FALSE)
      }
      else
      {print("something went wrong")}
    }
    for (i in 1:length(data))
    {
      if (l[i] == "NULL")
      {k <- l[ - which(l == "NULL")]}
      else {k <- append(l,l[i])
      k <- k[-which(duplicated(k))]}
    }
    kp = ncol(data) 
    j = 1
    for (i in 1:kp)
    {
      if (k[i] != "NULL")
      {
        colnames(data)[i] = colnames(data2)[k[[j]]]
      }
      else if (k[i] == "NULL") 
      {
        j = i+1
        colnames(data)[i] = colnames(data2)[k[[j]]]
        kp = kp + 1
      }
      j = j+1
    }
  }
  else if (texcol == "no" | texcol == "No" | texcol == "NO" | texcol == "0" | texcol == "n" | texcol == "false" | texcol == "False" | texcol == "FALSE")
  {
    data <- data2
    cat("you have decided not to remove text columns\n")
    for (x in 1:ncol(data))
    {
      if (is.character(data[,x]))
      {
        cat("column",x,"is a column containing text\n")
      }
    }
    
  }
  else 
  {
    data <- data2
    cat("you haven't responded to the question on whether to remove text columns\n")
    for (x in 1:ncol(data))
    {
      if (is.character(data[,x]))
      {
        cat("column",x,"is a column containing text\n")
      }
    }
  }
  
  if (NA_remove == "yes" | NA_remove == "Yes" | NA_remove == "YES"| NA_remove == "1" | NA_remove == "y" | NA_remove == "true" | NA_remove == "True" | NA_remove == "TRUE")
  {
    data <- na.omit(data)
    cat("You have decided to remove NAs & NULLs\n")
  }
  else if (NA_remove == "no" | NA_remove == "No" | NA_remove == "NO"| NA_remove == "0" | NA_remove == "n" | NA_remove == "false" | NA_remove == "False" | NA_remove == "FALSE")
  {
    data <- data
    cat("You have decided not to remove NAs & NULLs\n") 
  }
  else
  {
    data <- data
    cat("You haven't responded to the question on whether to remove NAs & NULLs")  
  }
  for (i in 1:ncol(data))
  {
    if (i == 1) {cat(paste("data loaded!!! \nyou can now choose between\n\n"))}
    cat(paste(colnames(data)[i], collapse = " "), "\n")
  }
  rm(data2, from_d, stocks, l, i, NA_remove, texcol, to_d, x)
  runprocess = 0
}

summary(data)