# Adil Gokturk
# FIN 659
# HW1 ESTIMATING VOLATILITY FROM HISTORICAL DATA
# Textbook Reference:			Section 15.4, pp. 323-327
# Set working directory
setwd("~/Desktop/Spring2020/FIN659/Assignments/hw1")
getwd()
# Load the libraries
library(quantmod)
library(tidyverse)
library(timeSeries)
library(fPortfolio)
library(caTools)
library(PerformanceAnalytics)
library(ggplot2)

## 1. Go to the Yahoo! Finance website, search for Apple Inc. (AAPL), and download the "Historical Data"
# Set the Time Period to __Dec 31, 2017 to Dec 31, 2018,__ set the Frequency to Daily, and click on the "Apply" button

#I used quantmod package to get data and set the date 
# from="2018-1-1", to="2019-1-1" to get all the 2018 trading days

getSymbols(Symbols = "AAPL", from="2018-1-1", to="2019-1-1")

# check the data
length(AAPL$AAPL.Adjusted)
head(AAPL)
tail(AAPL)

##2. Calculate continuously compounded returns for each daily interval, 
## using the function LN(Today's Adj Close / Yesterday's Adj Close)

apple2018 <- dailyReturn(x = AAPL$AAPL.Adjusted) %>% round(4)

  
# let's take a look at the first and last 6 days' return
head(apple2018)
tail(apple2018)

# remove the ZERO valued First day form the data
apple2018 <- apple2018[-1,]

# let's check it
head(apple2018)

# removed the first day

# check the structure
str(apple2018) 

# xts allows to plot timeseries data
# Let's take a look at the daily volatility of the Apple stock price in 2018
plot(apple2018)

## 3. Calculate the standard deviation of the returns,
## using the function STDEV across all returns

  # standard deviation of the returns
apple2018.sd <- sd(apple2018)
apple2018.sd
(apple2018.sd *100) %>% round(2) # Percentage %

## Calculated standard deviation of the returns= 0.0181071952637973 or
## 1.81 %

## 4. Estimate the annualized historical volatility, 
## by multiplying the standard deviation of daily returns 
## by the square root of the number of trading days in a year (252)

apple2018.hist.volatility <- apple2018.sd*sqrt(252)
apple2018.hist.volatility
(apple2018.hist.volatility*100) %>% round(2) # Percentage %

## Annualized historical volatility = 28.74 %

## 5. Estimate the annualized historical volatility for the last 3 months of the year only,
## by combining Steps 3 and 4 and applying them to the relevant returns

#  Let's get the last 3 months return data
length(apple2018) # 2018 data
length(apple2018[188:250,]) # last 3 months
head(apple2018[188:250,]) # Let's check it
tail(apple2018[188:250,])

# Calculate the  annualized historical volatility for the last 3 months of the year
(apple2018.quarter4.volatility <- sd(apple2018[188:250,])*sqrt(252))
(apple2018.quarter4.volatility *100) %>% round(2)

## The annualized historical volatility for the last 3 months of the year = 40.22%
## It is slightly different than the solution of 40.42! 
## I assume R still use more than 4 decimal to calculate.

## Which estimated annualized historical volatility is greater: 
## that for the entire year, or that for the last quarter?¶

# Answer: Last Quarter


## What is the most likely explanation for your previous answer?
# Answer: B. Uncertainty related to Apple stock prices increased 
## during the last quarter of the year!

## I found another R package--PerformanceAnalytics, 
## I think it minimizes algebra usage in Financial analytics.
  
  