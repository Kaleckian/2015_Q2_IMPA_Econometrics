setwd("C:/Users/guilherme.hossaka/wrkdir")  #Set folder as Working Directory.
#setwd("D:/R/wrkdir") #Set folder as Working Directory

rm(list=ls()) #Command to delete all R memory.

library("MASS")
library("tseries")
library("fExtremes")
library("rugarch")
library("fGarch")
library("sn")

set.seed(88)

Asset <- read.csv(file = "WEGE3.csv", sep = ";") #Read index series of WEGE3 in .csv from the Working Directory.

############### Extraction of Continuously Compounded Returns - S ###############
# Time Series: daily basis
# [NOTE 1]: ADJUSTED PRICE FOR DIVIDENDS AND INTERESTS ON NET EQUITY

mat_P <- Asset #Assign to "mat_P". All further references primarly to mat_P.
mat_P <- data.frame(mat_P[order(mat_P$Data),]) #Sort "mat_P" by column "Date".

############### Subsetting by indices - S ###############
index_1 <- which(mat_P$Data == "2007-05-30") #Find index of specific value at column "Date".
index_2 <- which(mat_P$Data == "2015-03-27") #Find index of specific value at column "Date".
mat_P <- (mat_P[index_1:index_2,]) #Subset by dates in indices for all columns.
############### Subsetting by indices - E ###############

mat_P <- mat_P[mat_P[,5] !="0",] #Test markets days by trading volume.

P <- mat_P[, 5, drop = F] #Extract "Adj. Close" from 5th column.

rownames(P) <- mat_P$Data #Assign dates as row labels of "P".

n <- nrow(P) #Assign number of observations/rows to "n".

Sret <- (P[2:n, 1, drop = F]-P[1:(n-1), 1, drop = F])/(P[1:(n-1), 1, drop = F]) #Calculate simple return.
ret <- (log(P[2:n, 1, drop = F]/P[1:(n-1), 1, drop = F])) #Calculate continously compounded return (log-return).

colnames(ret) <- c("CC_Return") #Name column of continously compoounded return.

############### Extraction of Continuously Compounded Returns - E ###############

############### Subsetting for Backtest: VaR (unconditional) - S ###############
ret_full <- ret

n_a <- round(n*.65)
n_b <- round(n*.35)

ret <- ret_full[1:n_a, , drop = F]
ret_b <-ret_full[(n_a+1):(n-1), , drop = F]

############### Subsetting for Backtest: VaR (unconditional) - E ###############

############### Statistical Briefing - S ###############

mean_ret <- mean(ret[,1]) #Mean of returns
var_ret <- var(ret[,1]) #Sample variance of returns: (n-1) as denominator.
sd_ret <- sd(ret[,1]) #Sample standard deviation of returns: (n-1) as denominator.
median_ret <- median(ret[,1]) #Sample median of returns.
max_ret <- max(ret[,1])
min_ret <- min(ret[,1])
skewn_ret <- skewness(ret[,1])
kurt_ret <- kurtosis(ret[,1])

plot(ret[,1], xlab = "Time", ylab = "CC Returns", type = "l", main = "CC Returns of WEGE3")

hist(ret[,1], breaks="fd", xlab = "CC returns", ylab = "Abs. Freq.", main = "Histogram of CC Returns of WEGE3")

#a) ACF of ret
acf(ret[,1], type = c("correlation"), xlab = "Lag", ylab = "CC Returns of WEGE3", main = "ACF of WEGE3 CC Returns")

#b) ACF of ret^2
acf(ret[,1]^2, type = c("correlation"), xlab = "Lag", ylab = "Squared CC Returns of WEGE3", main = "ACF of WEGE3 Squared CC Returns")

#Normality Tests

#a) JBS
Test_I <- jarque.bera.test(ret)

#b) SW
Test_II <- shapiro.test(ret[,1])

#c) KS
Test_III <- ks.test(ret[,1], pnorm(1, mean = mean_ret, sd = sd_ret))

#Weak Stationarity:
#a) KPSS - LEVEL
Test_IV <- kpss.test(ret[,1], null=("Level"))

#b) KPSS - TREND
Test_V <- kpss.test(ret[,1], null=("Trend"))

############### Statistical Briefing - E ###############

############### Value at Risk (unconditional) - S ###############

#I) Historical VaR: empirical distribution.
Hist_VaR <- data.frame(quantile(ret[,1], probs=c(0.05, 0.01, 0.001)))
rownames(Hist_VaR) <- c(0.05, 0.01, 0.001)
colnames(Hist_VaR) <- c("Hist_VaR")

#II) Assuming ret~N(mu,sigma^2): sample mean (mean_ret) and standard deviation (sd_ret).
Normal_VaR <- data.frame(qnorm(p = c(0.05, 0.01, 0.001), mean = mean_ret, sd = sd_ret, lower.tail = TRUE, log.p = FALSE))
rownames(Normal_VaR) <- c(0.05, 0.01, 0.001)
colnames(Normal_VaR) <- c("Normal_VaR")

#III) Uses T-Student distribution: "fat-tailed".
mle_T <- data.frame(fitdistr(ret[,1], densfun = "t", start = list(m = mean_ret, s = sd_ret, df = 3), lower = c(-Inf, 0))[1])
mean_T <-mle_T[1,]
sd_T <- mle_T[2,]
df_T <- mle_T[3,]

T_VaR <- ((data.frame(qt(p = c(0.05, 0.01, 0.001), df = df_T, lower.tail = TRUE, log.p = FALSE)) * sd_T) + mean_T)
rownames(T_VaR) <- c(0.05, 0.01, 0.001)
colnames(T_VaR) <- c("T_VaR")

##### Summarizing results

VaR_Results <- cbind(Hist_VaR, Normal_VaR, T_VaR)

############### Value at Risk (unconditional) - E ###############

############### Skewed Value at Risk (unconditional) - S ###############

#I) Skew Normal

mle_SNorm <- snormFit(ret[,1])
mean_SNorm <- mle_SNorm$par[1]
sd_SNorm <- mle_SNorm$par[2]
xi_SNorm <- mle_SNorm$par[3]

VaR_SNorm <- data.frame(qsnorm(p = c(0.05, 0.01, 0.001), mean = mean_SNorm, sd = sd_SNorm, xi = xi_SNorm))

rand_SNorm <- rsnorm(n = n_a, mean = mean_SNorm, sd = sd_SNorm, xi = xi_SNorm)

hist(rand_SNorm, breaks="fd", xlab = "CC returns", ylab = "Abs. Freq.", main = "Estimated Skew-Normal Histogram")

colnames(VaR_SNorm)<- c("VaR_SNorm")

VaR_Results <- cbind(VaR_Results, VaR_SNorm)

#II) Skew T-Student
mle_ST <- sstdFit(ret[,1])
mean_ST <- mle_ST$estimate[1]
sd_ST <- mle_ST$estimate[2]
nu_ST <- mle_ST$estimate[3]
xi_ST <- mle_ST$estimate[4]

VaR_ST <- data.frame(qsstd(p = c(0.05, 0.01, 0.001), mean = mean_ST, sd = sd_ST, nu = nu_ST, xi = xi_ST))

rand_ST <- rsstd(n = n_a, mean = mean_ST, sd = sd_ST, xi = xi_ST)

hist(rand_ST, breaks="fd", xlab = "CC returns", ylab = "Abs. Freq.", main = "Estimated Skew-T Histogram")

colnames(VaR_ST)<- c("VaR_ST")

VaR_Results <- cbind(VaR_Results, VaR_ST)

############### Skewed Value at Risk (unconditional) - E ###############

############### VaR (unconditional): Extreme Value Analysis - S ###############

ret_EVA <- ret*-1
ret_EVAb <- ret_b*-1

#I) GEV Distribution

len_block <- 22

list_gevFit <- gevFit(ret_EVA[1:((n_a%/%len_block)*len_block), , drop = F], block = len_block  ,type = "mle") 
xi_gevFit <- list_gevFit@fit$par.ests[1] #Shape
mu_gevFit <- list_gevFit@fit$par.ests[2] #Location
beta_gevFit <- list_gevFit@fit$par.ests[3] #Scale

data_gevFit <- list_gevFit@data$blockmaxima

hist(data_gevFit, breaks = "fd", xlab = "CC returns", ylab = "Abs. Freq.", main = "Histogram of GEV Distribution of Extreme Values")

VaR_GEV <- data.frame(qgev(p = c(0.95**len_block, 0.99**len_block, 0.999**len_block), xi = xi_gevFit, mu = mu_gevFit, beta = beta_gevFit)) * -1

colnames(VaR_GEV)<- c("VaR_GEV")

VaR_Results <- cbind(VaR_Results, VaR_GEV)

hist(data_gevFit, breaks = "sturges")

#II) GP Distribution

p_gpd = 0.9
list_gpdFit <- gpdFit(ret_EVA[,1], u = quantile(x = ret_EVA[,1], p = p_gpd, lower.tail = F), type = "mle")

xi_gpdFit <- list_gpdFit@fit$par.ests[1]
beta_gpdFit <- list_gpdFit@fit$par.ests[2]
mu_gpdFit <- list_gpdFit@parameter$u[1]

data_gpdFit <- list_gpdFit@data$exceedances

hist(data_gpdFit, breaks = "fd", xlab = "CC returns", ylab = "Abs. Freq.", main = "Histogram of GPD Distribution of Extreme Values")

VaR_GPD <- VaR_GPD <- (data.frame(qgpd(p = c(0.95 * p_gpd, 0.99 * p_gpd, 0.999 * p_gpd), xi = xi_gpdFit, mu = mu_gpdFit, beta = beta_gpdFit))) * -1

#VaR_GPD <- mu_gpdFit + (beta_gpdFit/xi_gpdFit)*(((0.05*(length(ret_EVA)/length(data_gpdFit)))**-xi_gpdFit)-1)

colnames(VaR_GPD)<- c("VaR_GPD")

VaR_Results <- cbind(VaR_Results, VaR_GPD)


############### VaR (unconditional): Extreme Value Analysis - E ###############

############### VaR (unconditional): Kupiec TEST - S ###############

Kupiec_Results <- data.frame(matrix(numeric(0), nrow=nrow(VaR_Results), ncol=ncol(VaR_Results)))

for (j in 1:ncol(Kupiec_Results)) {
  for (i in 1:nrow(Kupiec_Results)) {

      Kupiec_Results[i,j] <- VaRTest(alpha = as.numeric(row.names(VaR_Results)[i]), VaR = VaR_Results[i,j], actual = ret_b)$uc.Decision
  }
}

Kupiec_Results[Kupiec_Results == NA] <- "H0 Acc"

colnames(Kupiec_Results) <- colnames(VaR_Results)
rownames(Kupiec_Results) <- rownames(VaR_Results)

hist(ret_b[,1], breaks="fd", xlab = "CC returns", ylab = "Abs. Freq.", main = "Histogram of Actual CC Returns of WEGE3 for Backtesting")

############### VaR (unconditional): Kupiec TEST - E ###############

############### VaR (unconditional): Expected Shortfall - S ###############

ES_VaR <- data.frame(matrix(numeric(0), nrow=nrow(VaR_Results), ncol=ncol(VaR_Results)))
colnames(ES_VaR) <- colnames(VaR_Results)
rownames(ES_VaR) <- rownames(VaR_Results)

obsVaR1 <- ret[ret[,1] <= VaR_Results[1,1],]
ES_VaR1 <- mean(obsVaR1)

obsVaR2 <- ret[ret[,1] <= VaR_Results[2,1],]
ES_VaR2 <- mean(obsVaR2)

obsVaR3 <- ret[ret[,1] <= VaR_Results[3,1],]
ES_VaR3 <- mean(obsVaR3)

hist(obsVaR1, breaks = ("sturges"), xlab = "CC returns", ylab = "Abs. Freq.", main = "Histogram of CC Returns =< Historical VaR_0.05")
hist(obsVaR2, breaks = ("sturges"), xlab = "CC returns", ylab = "Abs. Freq.", main = "Histogram of CC Returns =< Historical VaR_0.01")
hist(obsVaR3, breaks = ("sturges"), xlab = "CC returns", ylab = "Abs. Freq.", main = "Histogram of CC Returns =< Historical VaR_0.001")

############### VaR (unconditional): Expected Shortfall - E ###############

#rm(list=ls()) #Command to delete all R memory.