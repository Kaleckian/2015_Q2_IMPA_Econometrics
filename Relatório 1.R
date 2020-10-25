setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#alll3 <- read.csv(file = "alll3.csv") #Read return series of ALLL in .csv from the Working Directory.
abev3 <- read.csv(file = "abev3.csv") #Read return series of ALLL in .csv from the Working Directory.
petr4 <- read.csv(file = "petr4.csv") #Read return series of ALLL in .csv from the Working Directory.

##### METADATA OF ALLL3 AND ABEV3
# 3389 OBS. Time Series: daily basis from 2000-01-05 to 2015-03-20
# [NOTE 1]: ADJUSTED PRICE FOR DIVIDENDS AND INTERESTS ON NET EQUITY
# COL1 - $Date: DATE - FORMAT YYYY-MM-DD
# COL2 - $Open: OPENING PRICE
# COL3 - $High: DAY HIGHEST PRICE
# cOL4 - $Low: DAY LOWEST PRICE
# COL5 - $Close: DAY CLOSURE PRICE
# COL6 - $Volume: VOLUME OF TRADES - # OF SHARES
# COL7 - $Adj. Close: DAY ADJUSTED CLOSURE PRICE [1]
#### Source: Yahoo Finance - Brazil

###mkt_days <- petr4 #Use petr4 trading volume as market days reference.

###mkt_days <- data.frame(mkt_days[order(mkt_days$Date),]) #Sort "mktdays" by column "Date".

###mkt_days <- data.frame(cbind((mkt_days[,1, drop = F]), (mkt_days[,6, drop = F]))) #Extract date and PETR4 trading volume as reference.

#Implement indices for min and max days. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

mat_P <- abev3 #Focus on ABEV3 by assigning to "mat_P". All script further references primarly to mat_P.

mat_P <- data.frame(mat_P[order(mat_P$Date),]) #Sort "mat_P" by column "Date".

###min_date <- min(as.Date(mat_P[,1])) #Find earliest observation of "mat_P".
###max_date <- max(as.Date(mat_P[,1])) #Find lastest observation of "mat_P".

###index_min <- which(mkt_days == data.frame(min_date))
###index_max <- which(mkt_days == data.frame(max_date))

###mkt_days <- (mkt_days[index_min:index_max, , drop=F]) #Subset by dates in indices and all columns.

###colnames(mkt_days) <- c("Date", "mktdays")

###mat_P <- data.frame(cbind(mat_P, mkt_days[,1]))

###mat_P <- subset(mat_P, mktdays !=0) #Subset of market days with criteria Volume != 0.

index_1 <- which(mat_P$Date == "2000-01-05") #Find index of specific value at column "Date".
index_2 <- which(mat_P$Date == "2015-03-20") #Find index of specific value at column "Date".

#mat_P <- (mat_P[index_1:index_2,]) #Subset by dates in indices for all columns.

mat_P <- mat_P[mat_P[,6] !=0,]

P <- mat_P[, 7, drop = F] #Extract "Adj. Close" from 7th column.

rownames(P) <- mat_P$Date #Assign dates as row labels of "P".

n <- nrow(P) #Assign number of observations/rows to "n".

Sret <- (P[2:n, 1, drop = F]-P[1:(n-1), 1, drop = F])/(P[1:(n-1), 1, drop = F]) #Calculate simple return.
ret <- (log(P[2:n, 1, drop = F]/P[1:(n-1), 1, drop = F])) #Calculate continously compounded return (log-return).

colnames(ret) <- c("CC_Return")

#Summarizing ret



#Some Stylized Facts
#I - Weak Stationarity:
#a) KPSS - LEVEL
kpss.test(ret[,1], null=("Level"))

#b) KPSS - TREND
kpss.test(ret[,1], null=("Trend"))

#II - ACF

#a) ACF of ret

#b) ACF of ret^2

#III - Normality Tests



#rm(list = ls(all = TRUE)) #Command to delete all R memory.
