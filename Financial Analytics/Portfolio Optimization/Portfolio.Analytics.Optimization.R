## Week 9 - Portfolio Analytics
setwd("/Users/HAG/Desktop/Spring2020/FIN654/Weeks/w6")
getwd()
library(tidyverse)
require(qrmdata)
require(xts)
library(PerformanceAnalytics)
# The exchange rate data was obtained
# from OANDA (http://www.oanda.com/)
# on 2016-01-03

#Value at risk (VaR) is the alpha quantile of portfolio value 
# where (“alpha”) is the organization’s tolerance for risk.
#VaR is the maximum amount of tolerable loss. But more loss is possible.

## We let R stand for returns, so that
# −R is a loss. Our management team wants
#           Prob(R < −0.10) = 0.05,
# that is, the probability of a loss worse than 10 % is no more than 5 %.
# Let w be the “weight” invested in the risky contract.
# The rest, 1 − w in high quality collateral assets like treasury bonds.
# The weight w can take on values from 0% (0.0) to 100% (1.0).
# No collateral means w = 1; no contract means w = 0.

## The average return, μ, on the contract is
# μ = w(0.1) + (1 − w)(0.02).
# This is the weighted average return of the contract at 10% 
#and collateral at 2%.
# The average level of risk in this model is given by the standard deviation of
# this combination of risky contract and default-free collateral.

## Management currently believes that a 25% standard deviation, “sigma” ,
# is reasonable.
# sigma= w^2(0.25)2 + (1 − w)^2 * (0.0)
# Collateral is not “risky” in this scenario.

qnorm(.05)
R <- -.12/(.25 * (-1.64) + .12)
(R * 100) %>% round(2)  # 42% of portfolio value = risky contract value.


# Risky contract + collateral = portfolio value.
(portfolio= 1000000000/R)
(collateral.value <- portfolio-1000000000)

## Markowitz (1952) portfolio solution

require(qrmdata)
require(xts)
# The exchange rate data was obtained
# from OANDA (http://www.oanda.com/)
# on 2016-01-03
data("EUR_USD")
data("GBP_USD")

# The Brent data was obtained from
# Federal Reserve Economic Data
# (FRED) via Quandl on 2016-01-03

data("OIL_Brent")
data.1 <- na.omit((merge(EUR_USD, GBP_USD, OIL_Brent)))
R <- na.omit(diff(log(data.1)) * 100)
names.R <- c("EUR.USD", "GBP.USD","OIL.Brent")
colnames(R) <- names.R
(mean.R <- apply(R, 2, mean))
summary(R)

# Brent crude’s annualized mean return is calculated on a 252 average days traded in
#a year basis as:
(((1 - mean.R[3]/100)^252 - 1) *100) %>% round(2)

## Variance - coVariance Matrix
(cov.R <- cov(R))

(sd.R <- sqrt(diag(cov.R)))


## Calculate the portfolio mean return.
## Calculate the portfolio standard deviation

sigma <- c(0.2, 0.15, .4)
rho <- c(1, .2, .6, .2, 1, .4, .6, .4, 1)
(rho <- matrix(data = rho, nrow = 3, ncol = 3))

(Sigma <- (sigma %*% t(sigma)) * rho)

w <- c(-.5, -.5, 1)
mu <- c(.12, -.09, .15)
(mu.P <- t(w) %*% mu)

rho <- c(1, .2, .6, .2, 1, .4, .6, .4, 1)
(rho <- matrix(data = rho, nrow = 3, ncol = 3))

(Sigma2 <- (sigma %*% t(sigma)) *rho)

(Sigma.P <- (t(w) %*% Sigma2 %*% w))^.5

########################################
## Optimization Model

# To perform the optimization task we turn to the quadprog quadratic programming
# package (yes, parabolas are indeed very useful).!
library(quadprog)

Amat <- cbind(rep(1, 3), mean.R) # set the equality constraints matrix
mu.P <- seq(min(mean.R - 5e-04), max(mean.R +
                                       5e-04), length = 300) # set of 300 possible target portfolio returns
sigma.P <- mu.P # set up storage for std dev's of portfolio returns
weights <- matrix(0, nrow = 300, ncol = ncol(R)) # storage for portfolio weights
colnames(weights) <- names.R

# build the “efficient frontier.”

# This curve (a parabola. . . ) traces optimal combinations of risk and return.
# For each combination there is an underlying set of weights.
# In effect this is a very specialized sensitivity analysis

for (i in 1:length(mu.P)) {
  bvec = c(1, mu.P[i]) # constraint vector
  result = solve.QP(Dmat = 2 * cov.R,
                    dvec = rep(0, 3), Amat = Amat,
                    bvec = bvec, meq = 2)
  sigma.P[i] = sqrt(result$value)
  weights[i, ] = result$solution
}


# Plot all of the portfolio combinations.
# Plot the point on the graph that represents the so-called risk-free 
# (actuall more like default-free) asset.

par(mfrow = c(1, 1))
plot(sigma.P, mu.P, type = "l", xlim = c(0,
                                         max(sd.R) * 1.1), ylim = c(0, max(mean.R) *
                                                                      1.1), lty = 3, lwd = 3) # plot
# the efficient frontier (and
# inefficient portfolios below the
# min var portfolio)
mu.free = 1.3/253 # input value of risk-free interest rate
points(0, mu.free, cex = 1, pch = "+") # show risk-free asset

## William Sharpe’s ratio:

sharpe = (mu.P - mu.free)/sigma.P # compute Sharpe's ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe's ratio
options(digits = 3)
lines(c(0, 2), mu.free + c(0, 2) * (mu.P[ind] -
                                      mu.free)/sigma.P[ind], lwd = 4, lty = 1,
      col = "blue")
# show line of optimal portfolios
points(sigma.P[ind], mu.P[ind], cex = 4,
       pch = "*") # show tangency portfolio
ind2 = (sigma.P == min(sigma.P)) # find the minimum variance portfolio
points(sigma.P[ind2], mu.P[ind2], cex = 2,
       pch = "+") # show min var portfolio
ind3 = (mu.P > mu.P[ind2]) # finally the efficient frontier
lines(sigma.P[ind3], mu.P[ind3], type = "l",
      xlim = c(0, max(sd.R) * 1.1), ylim = c(min(mean.R) *
                                               1.05, max(mean.R) * 1.1), lwd = 3,
      col = "red") # plot the efficient frontier
text(sd.R[1], mean.R[1], "EUR.USD", cex = 1.15)
text(sd.R[2], mean.R[2], "GBP.USD", cex = 1.15)
text(sd.R[3], mean.R[3], "OIL_Brent",
     cex = 1.15)

## For a given notional amount in your portfolio, go long (buy) 250.% of that position in euros
#traded against USD, go short (sell) 180.7% of your aggregate position in euros traded against
# USD, and go long 30.6% in Brent.


## Here is the new setup code where we no longer allow for short positions.

Amat <- cbind(rep(1, 3), mean.R, diag(1,
                                      nrow = 3)) # set the equality ND inequality constraints matrix
mu.P <- seq(min(mean.R) + 1e-04, max(mean.R) -
              1e-04, length = 300) # set of 300 possible target portfolio returns
sigma.P <- mu.P # set up storage for std dev's of portfolio returns
weights <- matrix(0, nrow = 300, ncol = 3) # storage for portfolio weights



## Next we build the “efficient frontier.” All of this code is as before.
for (i in 1:length(mu.P)) {
  bvec <- c(1, mu.P[i], rep(0, 3)) # constraint vector with no short positions
  result <- solve.QP(Dmat = 2 * cov.R,
                     dvec = rep(0, 3), Amat = Amat,
                     bvec = bvec, meq = 2)
  sigma.P[i] <- sqrt(result$value)
  weights[i, ] <- result$solution
}

## Then plot away. . . again the same as before
# 1 Plot all of the portfolio combinations.
# 2 Plot the point on the graph that represents
#the so-called risk-free (actually more like default-free) asset.

par(mfrow = c(1, 1))
plot(sigma.P, mu.P, type = "l", xlim = c(0,
                                         max(sd.R) * 1.1), ylim = c(min(mean.R) *
                                                                      1.05, max(mean.R) * 1.1), lty = 3,
     lwd = 3) # plot the efficient frontier (and inefficient portfolios
# below the min var portfolio)
mu.free <- 1.3/253 # input value of risk-free interest rate
points(0, mu.free, cex = 1.5, pch = "+") # show risk-free asset

## Here is the code (again)
sharpe <- (mu.P - mu.free)/sigma.P # compute Sharpe's ratios
ind <- (sharpe == max(sharpe)) # Find maximum Sharpe's ratio
options(digits = 3)
lines(c(0, 2), mu.free + c(0, 2) * (mu.P[ind] -
                                      mu.free)/sigma.P[ind], lwd = 4, lty = 1,
      col = "blue")
# show line of optimal portfolios
points(sigma.P[ind], mu.P[ind], cex = 4,
       pch = "*") # show tangency portfolio
ind2 <- (sigma.P == min(sigma.P)) # find the minimum variance portfolio
points(sigma.P[ind2], mu.P[ind2], cex = 2,
       pch = "+") # show min var portfolio
ind3 <- (mu.P > mu.P[ind2]) # finally the efficient frontier
lines(sigma.P[ind3], mu.P[ind3], type = "l",
      xlim = c(0, max(sd.R) * 1.1), ylim = c(min(mean.R) *
                                               1.05, max(mean.R) * 1.1), lwd = 3,
      col = "red") # plot the efficient frontier
text(sd.R[1], mean.R[1], "GE", cex = 1.15)
text(sd.R[2], mean.R[2], "IBM", cex = 1.15)
text(sd.R[3], mean.R[3], "Mobil", cex = 1.15)

# bvec changes for each of the three assets. Here we see one of them.
# The short position bvec has three zeros appended to it.
# The Amat constraint matrix has the identity matrix appended to it to represent wi = 0 in the
# formulation of the inequality constraints parsed by quantprog.
# The tangency of the line from the risk-free rate to the maximum Sharpe ratio
# point on the efficient frontier does not change.














