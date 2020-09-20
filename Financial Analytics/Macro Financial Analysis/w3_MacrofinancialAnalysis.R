#############################
## w3_Macrofinancial Analysis
#############################
getwd()
library(ggplot2)
library(IRdisplay)
library(psych)
library(GGally)
library(tidyverse)
library(fBasics)
library(evir)
library(qrmdata)
library(zoo)

# OIL data
data(OIL_Brent)
str(OIL_Brent)

# Let's create brent price as an object
Brent.price <- as.zoo(OIL_Brent)
Brent.return <- diff(log(Brent.price))[-1] *100

colnames(Brent.return)<-"Brent.return"
head(Brent.return, n = 5)

tail(Brent.return, n=5)

# Autocorrelation plots will reveal how persistent the returns are over time.
plot(Brent.return, title = FALSE, xlab = "",
     main = "Brent Daily % Change", col = "blue")
boxplot(as.vector(Brent.return), title = FALSE,
        main = "Brent Daily % Change", col = "blue",
        cex = 0.5, pch = 19)
skewness(Brent.return)
kurtosis(Brent.return)

# Now to look at persistence for 20 days:
acf(coredata(Brent.return), main = "Brent Daily Autocorrelogram",
    lag.max = 20, ylab = "", xlab = "",
    col = "blue", ci.col = "red")
pacf(coredata(Brent.return), main = "Brent Daily Partial Autocorrelogram",
     lag.max = 20, ylab = "", xlab = "",
     col = "blue", ci.col = "red")
# Now to look at persistence for 60 days:
acf(coredata(Brent.return), main = "Brent Daily Partial Autocorrelogram",
    lag.max = 60, ylab = "", xlab = "",
    col = "blue", ci.col = "red")

Brent.return.abs <- abs(Brent.return)
# Trading position size matters
Brent.return.tail <- tail(Brent.return.abs[order(Brent.return.abs)],
                          100)[1]
# Take just the first of the 100
# observations and pick the first
index <- which(Brent.return.abs > Brent.return.tail,
               arr.ind = TRUE)
# Build an index of those sizes that
# exceed the heavy tail threshold
Brent.return.abs.tail <- timeSeries(rep(0,
                                        length(Brent.return)), charvec = time(Brent.return))
# just a lot of zeros we will fill up
# next
Brent.return.abs.tail[index, 1] <- Brent.return.abs[index]
# A Phew! is in order
plot(Brent.return.abs, xlab = "", main = "Brent Daily Return Sizes",
     col = "blue")
acf(coredata(Brent.return.abs), main = "Brent Autocorrelogram",
    lag.max = 60, ylab = "", xlab = "",
    col = "blue", ci.col = "red")

pacf(coredata(Brent.return.abs), main = "Brent Partial Autocorrelogram",
     lag.max = 60, ylab = "", xlab = "",
     col = "blue", ci.col = "red")


# 3.4 Exercise: Getting Caught in the Crosscurrent

require(zoo)
require(qrmdata)
require(xts)
data("EuStockMarkets")
EuStockMarkets.price <- as.zoo(EuStockMarkets)
EuStockMarkets.return <- diff(log(EuStockMarkets.price))[-1] *
  100
# Plot the levels and returns.
plot(EuStockMarkets.price, xlab = " ",
     main = " ")

plot(EuStockMarkets.return, xlab = " ", main = " ")

ccf(EuStockMarkets.return[, 1], EuStockMarkets.return[,2],
    main = "Returns DAX vs. CAC",
    lag.max = 20, ylab = "", xlab = "",
    col = "blue", ci.col = "red")

ccf(abs(EuStockMarkets.return[, 1]),
    abs(EuStockMarkets.return[, 2]),
    main = "Absolute Returns DAX vs. CAC",
    lag.max = 20, ylab = "", xlab = "",
    col = "blue", ci.col = "red")

corr.rolling <- function(x) {
  dim <- ncol(x)
  corr.r <- cor(x)[lower.tri(diag(dim),
                             diag = FALSE)]
  return(corr.r)
}

corr.returns <- rollapply(EuStockMarkets.return, width = 250,
                          corr.rolling, align = "right", by.column = FALSE)

colnames(corr.returns) <- c("DAX & CAC", "DAX & SMI",
                            "DAX & FTSE", "CAC & SMI",
                            "CAC & FTSE", "SMI & FTSE")
# Let's plot the correlation returns
plot(corr.returns, xlab = "", main = "")

# Fisher Transformation
fisher <- function(r){
  0.5 * log((1 + r)/(1 - r))
} 


# Exercise 2
require(xts)
require(qrmdata)
require(quantreg)
require(quantmod)
require(matrixStats)

tickers <- c("ELE.MC", "IBE.MC", "REP.MC")
getSymbols(tickers)


REP.r <- diff(log(REP.MC[, 4]))[-1]
IBE.r <- diff(log(IBE.MC[, 4]))[-1]
ELE.r <- diff(log(ELE.MC[, 4]))[-1]

ALL.r <- merge(REP = REP.r, IBE = IBE.r,
               ELE = ELE.r, all = FALSE)

plot(ALL.r)

par(mfrow = c(2, 1))
acf(ALL.r)

par(mfrow = c(2, 1))
acf(abs(ALL.r))

par(mfrow = c(2, 1))
pacf(ALL.r)

par(mfrow = c(2, 1))
pacf(abs(ALL.r))

## Examine the correlation structure of the market
R.corr <- apply.monthly(ALL.r, FUN = cor)
R.vols <- apply.monthly(ALL.r, FUN = colSds) # from MatrixStats
head(R.corr, 3)

head(R.vols, 3)

R.corr.1 <- matrix(R.corr[1, ], nrow = 3,
                   ncol = 3, byrow = FALSE)
rownames(R.corr.1) <- tickers
colnames(R.corr.1) <- tickers
head(R.corr.1)

R.corr <- R.corr[, c(2, 3, 6)]
colnames(R.corr) <- c("ELE.IBE", "ELE.REP",
                      "IBE.REP")
colnames(R.vols) <- c("ELE.vols", "IBE.vols",
                      "REP.vols")
head(R.corr, 3)

head(R.vols, 3)

R.corr.vols <- merge(R.corr, R.vols)

plot.zoo(merge(R.corr.vols))

ELE.vols <- as.numeric(R.corr.vols[,
                                   "ELE.vols"])
IBE.vols <- as.numeric(R.vols[, "IBE.vols"])
REP.vols <- as.numeric(R.vols[, "REP.vols"])
length(ELE.vols)

fisher <- function(r) {
  0.5 * log((1 + r)/(1 - r))
}

rho.fisher <- matrix(fisher(as.numeric(R.corr.vols[,1:3])), 
                     nrow = length(ELE.vols),
                     ncol = 3, byrow = FALSE)
library(quantreg)

# Qunatile regression
taus <- seq(0.05, 0.95, 0.05) # upper and lower bound
fit.rq.ELE.IBE <- rq(rho.fisher[, 1] ~
                       ELE.vols, tau = taus)
fit.lm.ELE.IBE <- lm(rho.fisher[, 1] ~
                       ELE.vols)

plot(summary(fit.rq.ELE.IBE), parm = "ELE.vols")


# Here we build the estimations and plot the upper and lower bounds.
taus1 <- c(0.05, 0.95) # fit the confidence interval (CI)
plot(ELE.vols, rho.fisher[, 1], xlab = "ELE.vol",
     ylab = "ELE.IBE")
abline(fit.lm.ELE.IBE, col = "red")
for (i in 1:length(taus1)) {
  # these lines will be the CI
  abline(rq(rho.fisher[, 1] ~ ELE.vols,
            tau = taus1[i]), col = "blue")
}
grid()


# 3.5 Exercise: Time Is on Our Side


# Macrofinancial Data Analysis

name <- "GNP"
URL <- paste("http://research.stlouisfed.org/fred2/series/",
             name, "/", "downloaddata/", name,
             ".csv", sep = "")
download <- read.csv(URL)
par(mfrow = c(1, 1))
hist(download[,2])

summary(download[, 2])

GNP <- ts(download[1:84, 2]/1000, start = c(1995, 1), freq = 4) #quarters
GNP.rate = 100 * diff(log(GNP))

plot(GNP, type = "l", main = "US GNP Level")

plot(GNP.rate, type = "h", main = "GNP quarterly growth rates")
abline(h = 0, col = "darkgray")

par(mfrow = c(2, 1)) #stacked up and down
acf(GNP.rate)
acf(abs(GNP.rate))

par(mfrow = c(2, 1))
pacf(GNP.rate)
pacf(abs(GNP.rate))
par(mfrow = c(1, 1)) #default setting

################################################
# ARIMA-- Autoregressive Integrated Moving Average.
###################################################
##DIDNOT WORK
#fit.rate <- arima(GNP.rate, order = c(2, 0, 1))
#fit.rate

fit.rate.2 <- arima(GNP.rate, order = c(2,0, 0))
                                       
fit.rate.2

GNP.resid <- resid(fit.rate.2)
hist(GNP.resid, probability = TRUE, breaks = "FD",
     xlim = c(-1.5, 1.5), col = "steelblue",
     border = "white")
x = seq(-2, 2, length = 100)
lines(x, dnorm(x, mean = mean(GNP.resid),
               sd = sd(GNP.resid)), col = "orange",
      lwd = 2)

qqnorm(GNP.resid)
qqline(GNP.resid, col="red")


# Diagnose the GNP residuals using ACF and the moments package to calculate
#skewness and kurtosis.
# Try this
par(mfrow = c(2, 1))
pacf(GNP.rate)
pacf(abs(GNP.rate))
par(mfrow = c(1, 1)) #default setting

acf(GNP.resid)

# Nice absolute values (i.e., GNP growth sizes):
acf(abs(GNP.resid))

require(moments)

skewness(GNP.resid)
kurtosis(GNP.resid)

## Forecast
(GNP.pred <- predict(fit.rate.2, n.ahead = 8))

## bootstrapping
#3.6 Exercise: Give It the Boot


acf(REP.r, 1)# not so much

set.seed(1016)
acf.coeff.sim <- replicate(2500, acf(sample(REP.r,
                                            size = 2500, replace = FALSE), lag = 2,
                                     plot = FALSE)$acf[2])
summary(acf.coeff.sim)

hist(acf.coeff.sim, probability = TRUE,
     breaks = "FD", xlim = c(0.04, 0.05),
     col = "steelblue", border = "white")

# 95%
quantile(acf.coeff.sim, probs = c(0.025,
                                  0.975))

# 99%
# At 99% tolerance level
quantile(acf.coeff.sim, probs = c(0.005,
                                  0.995))

# And the
(t.sim <- mean(acf.coeff.sim)/sd(acf.coeff.sim))

# Plot the simulated density and lower and upper quantiles,
#along with the estimate of the lag-1 coefficient:
plot(density(acf.coeff.sim), col = "blue")
abline(v = 0)
abline(v = quantile(acf.coeff.sim, probs = c(0.025,
                                             0.975)), lwd = 2, col = "red")
abline(v = acf(REP.r, 1, plot = FALSE)$acf[2],
       lty = 2, lwd = 4, col = "orange")






