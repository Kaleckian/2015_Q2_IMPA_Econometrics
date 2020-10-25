#install.packages('aTSA')
library(aTSA)
library(tseries)
library(rugarch)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Set folder as Working Directory.
#setwd("D:/R/wrkdir") #Set folder as Working Directory

ibov <- read.csv(file = "ibov.csv") #Read index series of ibov in .csv from the Working Directory.

##### METADATA
# 3389 OBS. Time Series: daily basis from 2000-01-05 to 2015-03-20
# [NOTE 1]: ADJUSTED PRICE FOR DIVIDENDS AND INTERESTS ON NET EQUITY
# COL1 - $Date: DATE - FORMAT YYYY-MM-DD
# COL2 - $Open: OPENING PRICE
# COL3 - $High: DAY HIGHEST PRICE
# cOL4 - $Low: DAY LOWEST PRICE
# COL5 - $Close: DAY CLOSURE PRICE
# COL6 - $Volume: VOLUME OF TRADES - # OF SHARES
# COL7 - $Adj. Close: DAY ADJUSTED CLOSURE PRICE [1]
# Source: Yahoo Finance - Brazil

mat_P <- ibov #Focus on ABEV3 by assigning to "mat_P". All further references primarly to mat_P.
mat_P <- data.frame(mat_P[order(mat_P$Date),]) #Sort "mat_P" by column "Date".

############### Subsetting by indices ###############
#index_1 <- which(mat_P$Date == "2007-08-03") #Find index of specific value at column "Date".
#index_2 <- which(mat_P$Date == "2015-03-20") #Find index of specific value at column "Date".
#mat_P <- (mat_P[index_1:index_2,]) #Subset by dates in indices for all columns.
############### Subsetting by indices ###############

#mat_P <- mat_P[mat_P[,6] !=0,] #Test markets days by trading volume.

P <- mat_P[, 7, drop = F] #Extract "Adj. Close" from 7th column.

rownames(P) <- mat_P$Date #Assign dates as row labels of "P".

n <- nrow(P) #Assign number of observations/rows to "n".

Sret <- (P[2:n, 1, drop = F]-P[1:(n-1), 1, drop = F])/(P[1:(n-1), 1, drop = F]) #Calculate simple return.
ret <- (log(P[2:n, 1, drop = F]/P[1:(n-1), 1, drop = F])) #Calculate continously compounded return (log-return).

colnames(ret) <- c("CC_Return") #Name column of continously compounded return.

##############################
mean_ret <- mean(ret[,1]) #Mean of returns
var_ret <- var(ret[,1]) #Sample variance of returns: (n-1) as denominator.
sd_ret <- sd(ret[,1]) #Sample standard deviation of returns: (n-1) as denominator.
median_ret <- median(ret[,1]) #Sample median of returns.

plot(ret[,1], xlab = "Time", ylab = "CC Returns", type = "l", main = "CC Returns of AMBEV ON")

hist(ret[,1], xlim = c(0.1,-0.1), breaks="fd", xlab = "Abs. Freq.", ylab = "CC returns", main = "Histogram of CC Returns of IBOVESPA")

##############################
#Some Stylized Facts
#I - Weak Stationarity:
#a) KPSS - LEVEL
kpss.test(ret[,1], null=("Level"))

#b) KPSS - TREND
kpss.test(ret[,1], null=("Trend"))

#II - ACF

#a) ACF of ret
acf(ret[,1], type = c("correlation"), xlab = "Lag", ylab = "CC Returns of ABEV3", main = "ACF funcion of CC Returns of ABEV3")

#b) ACF of ret^2
acf(ret[,1]^2, type = c("correlation"), xlab = "Lag", ylab = "Squared CC Returns of ABEV3", main = "ACF funcion of Squared CC Returns of ABEV3")

#III - Normality Tests

#a) JBS
jarque.bera.test(ret)

#b) SW
shapiro.test(ret[,1])

#c) KS
ks.test(ret[,1], pnorm(1, mean = mean_ret, sd = sd_ret))

##############################
#Value at Risk (unconditional) #p-values: c("0.05", "0.01", "0.001")

#I) Historical VaR: empirical distribution.
quantile(ret[,1], probs=c(0.05, 0.01, 0.0001))

#II) Assuming ret~N(mu,sigma^2): sample mean (mean_ret) and standard deviation (sd_ret).
qnorm(0.05, mean = mean_ret, sd = sd_ret, lower.tail = TRUE, log.p = FALSE)
qnorm(0.01, mean = mean_ret, sd = sd_ret, lower.tail = TRUE, log.p = FALSE)
qnorm(0.001, mean = mean_ret, sd = sd_ret, lower.tail = TRUE, log.p = FALSE)

#III) Uses T-Student distribution: "fat-tailed".
mean_ret + sd_ret * qt(0.05, df = 7)
mean_ret + sd_ret * qt(0.01, df = 7)
mean_ret + sd_ret * qt(0.001, df = 7)

#rm(list = ls(all = TRUE)) #Command to delete all R memory.
