# FIN659 - Binomial Option Pricing Models
# Adil Gokturk

# set the working directory and check it!
setwd("~/Desktop/Spring2020/FIN659/Assignments/hw9")
getwd()

# Load the libraries
library(qrmtools)
library(fOptions) # used this one 
library(jrvFinance)
library(derivmkts) # and this one
library(OptionPricing)

# Notes
# Black-Scholes uses continues time rather than discrete

# The binomial tree method is particularly appropriate
# when we're trying to price American options.

# Black-Scholes-Merton Model
# Black-Scholes-Merton is useful for pricing only European options.

# But the CRR ( Cox-Ross-Rubenstein) also known as binomial tree
# allows us to work backwards through a tree and modify 
# the value at each node if the intrinsic value at that point is 
# greater than the discounted expected payoff.

# I used Week9 Asynchronous's example to test R packages accuracy 

s=50 # Price of the underlying asset
k=48 # 	Strike price
v=0.30 # volatility -sigma
r=0.01 # 	Annual continuously-compounded risk-free interest rate
tt=0.50 # Time to maturity in years
d=0 # Dividend yield, annualized, continuously-compounded



# ACCURATE MODEL I
# I used derivmkts R Library

# Black Scholes Call option pricing model
bscall(s, k, v, r, tt, d)

## To check accuracy 
# following returns the same price as previous
assetcall(s, k, v, r, tt, d) - k*cashcall(s, k, v, r, tt, d)

#Black Scholes Put option pricing model
bsput(s, k, v, r, tt, d)


## return option prices for multiple  strikes prices!
# very helpfull
bsput(s, k=40:60, v, r, tt, d)

# Black Scholes call/put option prices
bscall(s, k, v, r, tt, d) # Black-Scholes call  price
bsput(s, k, v, r, tt, d) #  Black-Scholes put  price

# # Also tryied some other function of the package
# the prices of binary options 
# that pay one share (the asset options) 
assetcall(s, k, v, r, tt, d)
cashcall(s, k, v, r, tt, d)
assetput(s, k, v, r, tt, d)
cashput(s, k, v, r, tt, d)

# Binomial European CAL option pricing
# I calculated w9 example's up and down values

# Define UP and DOWN values
(up <- exp(v*sqrt(tt/2))) # 1.1618
(dn <- exp(-v*sqrt(tt/2))) # 0.8607

# European CAL option
binomopt(s, k, v, r, tt, d, nstep = 2, american = FALSE,
         putopt = FALSE,specifyupdn = TRUE,
         jarrowrudd = FALSE, up = up, dn = dn,
         returntrees = FALSE,
         returngreeks = FALSE)

# European PUT option
binomopt(s, k, v, r, tt, d, nstep = 2, american = FALSE,
         putopt = TRUE,specifyupdn = TRUE,
         jarrowrudd = FALSE, up = up, dn = dn,
         returntrees = FALSE,
         returngreeks = FALSE)

# Binomial American (CRR "Cox-Ross-Rubenstein Model") CALL Option
binomopt(s, k, v, r, tt, d, nstep = 2, american = TRUE,
         putopt = FALSE,specifyupdn = TRUE,
         jarrowrudd = FALSE, up = up, dn = dn,
         returntrees = FALSE,
         returngreeks = FALSE)

# Binomial American PUT option pricing
binomopt(s, k, v, r, tt, d, nstep = 2, american = TRUE,
         putopt = TRUE,specifyupdn = TRUE,
         jarrowrudd = FALSE, up = up, dn = dn,
         returntrees = FALSE,
         returngreeks = FALSE)




# Let's plot the  American CAL option stock Tree
# Again we use  binomial opion pricing (crr model)  
# To plot different options manipulate "putop, american and crr" variables!
binomplot(s, k, v, r, tt, d, nstep = 2,
          putopt = FALSE, american = TRUE,
          plotvalues = TRUE, plotarrows = TRUE, 
          drawstrike = TRUE, pointsize = 4, ylimval = c(0,0),
          saveplot = FALSE, saveplotfn = 'binomialplot.pfd',
          crr = TRUE, jarrowrudd = FALSE,
          titles = TRUE,
          specifyupdn = TRUE,
          up = up, dn = dn,
          returnprice = FALSE,
          logy = FALSE) # If TRUE, y-axis is plotted on a log scale


##################################

## Second model
## fOptions R package
# The generalized Black-Scholes CALL Option Price
# b is the annualized cost-of-carry rate
# if no dividend b=r(Annual continuously-compounded risk-free interest rate)
GBSOption(TypeFlag = "c", S = s, X = k, Time = tt,
          r = r, b = r, sigma = v,
          title = NULL, description = NULL)

# The generalized Black-Scholes PUT Option Price
# no dividend b=r
GBSOption(TypeFlag = "p", S = s, X = k, Time = tt,
          r = r, b = r, sigma = v,
          title = NULL, description = NULL)@price

# to compare multiple strike price for BS call or put option
GBSOption(TypeFlag = "p", S = s, X = 40:48, Time = tt,
          r = r, b = r, sigma = v,
          title = NULL, description = NULL)@price

# CRR ( Cox-Ross-Rubenstein) Binomial Option
# European CALL option
# Accurate result
CRRBinomialTreeOption(TypeFlag = "ce", S = s, X = k,
                      Time = tt,
                      r = r, b = r,
                      sigma = v,n = 2)@price

# European PUT option
# Accurate result
CRRBinomialTreeOption(TypeFlag = "pe", S = s, X = k,
                      Time = tt,
                      r = r, b = r,
                      sigma = v,n = 2)@price
# American CALL option
# Accurate result
CRRBinomialTreeOption(TypeFlag = "ca", S = s, X = k,
                      Time = tt,
                      r = r, b = r,
                      sigma = v,n = 2)@price
# American PUT option
# Accurate result
CRRBinomialTreeOption(TypeFlag = "pa", S = s, X = k,
                      Time = tt,
                      r = r, b = r,
                      sigma = v,n = 2)@price

#########################################################
### European p/c option trees
#  Accurate results
# CRR Binomialtree Plots for European CALL Option
bintree <- BinomialTreeOption(TypeFlag = "ce", S = s, X = k,
                              Time = tt,
                              r = r, b = r,
                              sigma = v,n = 2)
# Plot the binomial tree option for European CALL Option
BinomialTreePlot(BinomialTreeValues = bintree, dy = 1, 
                 xlab = "Time Periods", 
                 ylab = "Number of Steps", 
                 main = "European Call Option Tree")

# CRR Binomialtree Plot for European PUT Option
bintree <- BinomialTreeOption(TypeFlag = "pe", S = s, X = k,
                              Time = tt,
                              r = r, b = r,
                              sigma = v,n = 2)
# Plot the binomial tree option for European PUT Option
BinomialTreePlot(BinomialTreeValues = bintree, dy = 1, 
                 xlab = "Time Periods", 
                 ylab = "Number of Steps", 
                 main = "European Put Option Tree")

### American p/c option trees
# CRR Binomialtree Plot for American CALL Option
bintree <- BinomialTreeOption(TypeFlag = "ca", S = s, X = k,
                              Time = tt,
                              r = r, b = r,
                              sigma = v,n = 2)
# Plot the binomial tree option for European CALL Option
BinomialTreePlot(BinomialTreeValues = bintree, dy = 1, 
                 xlab = "Time Periods", 
                 ylab = "Number of Steps", 
                 main = "American Call Option Tree")

# CRR Binomialtree Plot for  PUT Option
bintree <- BinomialTreeOption(TypeFlag = "pa", S = s, X = k,
                              Time = tt,
                              r = r, b = r,
                              sigma = v,n = 2)
# Plot the binomial tree option for American PUT Option
BinomialTreePlot(BinomialTreeValues = bintree, dy = 1, 
                 xlab = "Time Periods", 
                 ylab = "Number of Steps", 
                 main = "American Put Option Tree")





#####################################
#### Connection Between the mmodels
# goal is to create a risk free portfolio

# Major difference #
# binomial model is discrete where as 
# Black-Scholes-Merton Model is continues

# What is the correct hedging ratio
# partial derivatives of the option value 
# with respect to the underlying asset price (DELTA)

# need function!




