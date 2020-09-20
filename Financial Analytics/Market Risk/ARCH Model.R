## ACF-PACF in Finance
## Autocorrelation function (ACF) and partial autocorrelation (PACF)

# ARCH Models

library(zoo)
library(xts)
library(ggplot2)
library(tidyquant)
library(quantmod)

# Get the data
getSy
getSymbols(Symbols = "MCD", from="2010-1-1", to="2020-1-1")
head(MCD)
tail(MCD)

# Calcaulate daily return
bigmac <- dailyReturn(MCD$MCD.Adjusted)
head(bigmac)
tail(bigmac)

# remove the "ZERO" value--First day, form the data
bigmac <- bigmac[-1,]
head(bigmac)

str(bigmac)

# let's plot the daily volatility of McDonals' price
plot(bigmac)

#####################################
## ARCH Model -- Autoregressive conditional heteroskedasticity Model
# First Degree Autoregressive ARCH Model
# If there is no error term "E" it is not an ARCH model
# it is just a First Degree Autoregressive Model
# Y=M +N*X (no error term)
# First Degree Autoregressive ARCH Model
# Yt= M +N*X + Et-1 (Error term)

# Fit the ARCH model to McDonals data

# USE "rugarch" Library
library(rugarch)
head(bigmac)

# create a data framewith a single cariable called armaorder
# ARMA model
# AR - Autoregressive, value=1 means FIRST DEGREE
# MA - Moving Average value=0 


AO <- data.frame(armaOrder=c(1, 0))
AO

# create a data frame with garch model which is ERROR term ARCH TERM 
GO <- data.frame(garchOrder=c(1, 0))
GO

# define model specification
# fit the model a single variable
myspec <- ugarchspec(mean.model = AO, variance.model = GO)
myspec

# Fit the model
mymodel <- ugarchfit(spec = myspec, data = bigmac)

# look at the predicted coefficients of the model
coef(mymodel)

# simulate Future returns
# Long method by using loop
# we will use the last day of the data

# define the prediction coeffs
mu <- coef(mymodel)[1]
phi <- coef(mymodel)[2]
omega <- coef(mymodel)[3]
alpha <- coef(mymodel)[4]

mu
phi
omega
alpha

# 1000 simulation for 1000 simulated days

# create a matrix
Y <- matrix(0, nrow = 1000) # 1000 rows will be replaced with the simulated returns
n <- 1000
head(Y)
tail(Y)

# Let's create a matrix for ERROR TERMS for simulation
e <- matrix(0, nrow = 1000)
head(e)
tail(e)

# take the last day of the data 
tail(bigmac)
# simulation must start with the  last day of the data(bigmac) 2019-12-31
Y[1] <- tail(bigmac,1)
head(Y)

# Let's calsulate the residuals,residual=PREDICTED ERROR
myresiduals <- residuals(mymodel)

#First residual=ERROR  must start with the  last day of the data(bigmac) 2019-12-31
tail(myresiduals)
e[1] <- tail(myresiduals,1)

head(e)

## RUN the LOOP

for (t in 2:n) { # lower(2) and upper(1000) limit
  e[t] <- rnorm(1) * sqrt(omega + alpha*e[t-1]^2) # random ERROR term --  rnorm(1) throws ONE number
  Y[t] <- mu + phi*Y[t-1]+ e[t]
    
} 

head(Y)
tail(Y)

# simulated error terms
head(e)
tail(e)

# let plot the simulations

head(Y)
tail(Y)

# create date varaibale 
days <- (1:1000)
head(days)

# create df for ggplot2

resutlts <- data.frame(Y, days)
head(resutlts)

ggplot(resutlts, aes(x = days, y = Y)) +
  geom_line(color="magenta")+
  ylab("Simulated Daily returns")

# SHORT way of fitting the model by using rugarch package

  mysim2 <- ugarchsim(fit = mymodel, n.sim = 1000, startMethod  = "sample") # start simulation with the last day of the data
plot(mysim2)












