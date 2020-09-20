# Adil Gokturk
# FIN 659

# HW5 SWAPS
# Textbook Reference:		Section 7.5, pp. 165-167; Section 7.7, pp. 169-172

## Set working directory
setwd("/Users/HAG/Desktop/Spring2020/FIN659/Assignments/hw5")
getwd()

## Libraries
library(tidyverse)
library(quantmod)
library(ggplot2)
library(jrvFinance)
library(knitr)

#############
# Problem1 ##
#############
## The first key principle to take away from this problem is that 
## the swap rate at the outset of a contract is computed from 
## the observed term structure of interest rates at that time, 
## such that the initial value of the swap is zero to both parties involved.
## After the contract is initiated, however,
## the yield curve will change and the swap will have a positive value to one party and 
## a negative value to the other - a zero-sum game.
## It should be observed that 
## the floating rate is set at the beginning of each period, 
##but paid at the end - thus, 
## the first floating cash flow will be fixed at the outset of the swap.
## The second key principle to take away from this problem is to recognize that 
## swaps can be valued in one of two ways: 
## (i) as a portfolio of Forward Rate Agreements (FRAs), or 
## (ii) as the difference in the prices of a floating-rate bond and a fixed-rate bond.
## A third key principle to note is that a corporate treasurer is 
## likely to use an interest-rate swap 
## such as this one to change the nature of a liability or
## an asset - instead of paying (or receiving) a fixed rate,
## the financial manager can pay (or receive) a floating rate.						

## In a three-year interest rate swap, a financial institution agrees to pay 
##a fixed rate per annum and 
## to receive six-month LIBOR in return on a notional principal
##of $10 million with payments being exchanged every six months.

# Notional principal
(notional.principal <- 10000000)	#$10,000,000

# Time to maturity of swap (years)
(time.to.maturity.of.swap <- 3) # years

# Frequency (payments per year)
(frequency <-  	2) # payments per year	

## Complete the tables below in order to calculate the swap rate 
## when the financial institution enters into the contract. 
## (At this time, the value of the swap should be $0.)		

#################################
## Calculation of Forward Rates##
#################################
# Time
(time <- c(0.0, 0.5, 1.0,  1.5,  2.0,  2.5, 3.0))

# LIBOR Rates (sa)           
(LIBOR.rates.sa <-(c(3.25, 4.25, 4.95, 5.55, 5.95, 6.25))/100) #sa  

# LIBOR Rates (cc)
(LIBOR.rates.cc <-frequency * log(1+(LIBOR.rates.sa/frequency))) # cc

# Forward Rates (cc)
(forward.rates.cc <- c(((LIBOR.rates.cc[1] * time[2]) - (0 * time[1]))/(time[2] - time[1]),
                      ((LIBOR.rates.cc[2] * time[3]) - (LIBOR.rates.cc[1] * time[2]))/(time[3] - time[2]),
                      ((LIBOR.rates.cc[3] * time[4]) - (LIBOR.rates.cc[2] * time[3]))/(time[4] - time[3]),
                      ((LIBOR.rates.cc[4] * time[5]) - (LIBOR.rates.cc[3] * time[4]))/(time[5] - time[4]), 
                      ((LIBOR.rates.cc[5] * time[6]) - (LIBOR.rates.cc[4] * time[5]))/(time[6] - time[5]),
                      ((LIBOR.rates.cc[6] * time[7]) - (LIBOR.rates.cc[5] * time[6]))/(time[7] - time[6])))      
          
          
# Forward Rates (sa)    
(forward.rates.sa <- (frequency * (exp((forward.rates.cc)/frequency)- 1)))

# let's add ZEROs to values to make calculation easier
(LIBOR.rates.sa <- c(0,LIBOR.rates.sa))
(LIBOR.rates.cc <- c(0,LIBOR.rates.cc))
(forward.rates.cc <- c(forward.rates.cc, 0))
(forward.rates.sa <- c(forward.rates.sa, 0))

# Let's put all data in a table  
(analysis1.df <- data.frame(time, LIBOR.rates.sa, LIBOR.rates.cc, forward.rates.cc, forward.rates.sa))
kable(analysis1.df, align = "c")


## Calculation of Swap Rate
# At this time, the value of the swap should be 0
(swap.rate <- 0)

# Floating Cash Flow
(floating.cash.flow <- ((notional.principal * analysis1.df$forward.rates.sa[-7])/1) / frequency) # $

# Fixed Cash Flow
(fixed.cash.flow <- (swap.rate * notional.principal) / frequency)

# Net Cash Flow
(net.cash.flow <- floating.cash.flow - fixed.cash.flow)

# Discount Factor
(discount.factor <- (exp(-LIBOR.rates.cc[2:7] * time[2:7])))

# PV of Net Cash Flow
(PV.of.net.cash.flow <- net.cash.flow * discount.factor)

# Let's calculate the sum of PV of Net Cash Flow
(Sum.PV.of.net.cash.flow <- sum(PV.of.net.cash.flow))

# put it in a df
(Sum.PV.of.net.cash.flow <- as.data.frame(Sum.PV.of.net.cash.flow))


# Let's put all data in a data frame
(analysis2.df <- data.frame(floating.cash.flow, net.cash.flow, discount.factor, PV.of.net.cash.flow))

# this visualization would be better
kable(analysis2.df, align = "c")
kable(Sum.PV.of.net.cash.flow)

# Three months after the swap is initiated, the LIBOR rates have changed,
# as shown in the table below. 
# (Yields have shifted upwards, and the yield curve has flattened.) 

## Complete the tables below 
## to find the value of the swap at this time 
## from the financial institution's perspective.	

#####################################
## Calculation of New Forward Rates##
#####################################
# Time
(time <- c(0.0, 0.25, 0.75,  1.25,  1.75, 2.25,  2.75))

# LIBOR Rates (sa)
(LIBOR.rates.sa <-(c(5, 5.50, 5.85, 6.15, 6.35, 6.50))/100) #sa  

# LIBOR Rates (cc)
(LIBOR.rates.cc <-frequency * log(1+(LIBOR.rates.sa/frequency))) # cc

# Forward Rates (cc)
(forward.rates.cc <- c(((LIBOR.rates.cc[2] * time[3]) - (LIBOR.rates.cc[1] * time[2]))/(time[3] - time[2]),
                       ((LIBOR.rates.cc[3] * time[4]) - (LIBOR.rates.cc[2] * time[3]))/(time[4] - time[3]),
                       ((LIBOR.rates.cc[4] * time[5]) - (LIBOR.rates.cc[3] * time[4]))/(time[5] - time[4]), 
                       ((LIBOR.rates.cc[5] * time[6]) - (LIBOR.rates.cc[4] * time[5]))/(time[6] - time[5]),
                       ((LIBOR.rates.cc[6] * time[7]) - (LIBOR.rates.cc[5] * time[6]))/(time[7] - time[6])))      


# Forward Rates (sa)
(forward.rates.sa <- (frequency * (exp((forward.rates.cc)/frequency)- 1)))

# let's add ZEROs to values
(LIBOR.rates.sa <- c(0 ,LIBOR.rates.sa))
(LIBOR.rates.cc <- c(0,LIBOR.rates.cc))
(forward.rates.cc <- c(0,forward.rates.cc, 0))
(forward.rates.sa <- c(0,forward.rates.sa, 0))

# Let's put all data in a data frame
(analysis3.df <- data.frame(time, LIBOR.rates.sa, LIBOR.rates.cc, forward.rates.cc, forward.rates.sa))

# Visualize the df
kable(analysis3.df, align = "c")


## Calculation of Value of the Swap 
# At this time, the value of the swap should be 0
(swap.rate <- 0)

# Floating Cash Flow
# Need to get first Floating cash value from the previous analysys
(floating.cash.flow <- c(((analysis1.df$forward.rates.sa[1] * notional.principal) / frequency),
                         (notional.principal * analysis3.df$forward.rates.sa[2:6]) / frequency)) # $
# Fixed Cash Flow
(fixed.cash.flow <- (swap.rate * notional.principal) / frequency)

# Net Cash Flow
(net.cash.flow <- floating.cash.flow - fixed.cash.flow)

# Discount Factor
(discount.factor <- (exp(-LIBOR.rates.cc[2:7] * time[2:7])))

# PV of Net Cash Flow
(PV.of.net.cash.flow <- net.cash.flow * discount.factor)

# Let;s calculate the sum of the PV of Net Cash Flow
(Sum.PV.of.net.cash.flow <- sum(PV.of.net.cash.flow))

# Put it in a df
(Sum.PV.of.net.cash.flow <- as.data.frame(Sum.PV.of.net.cash.flow))

# # Let's put all the data in a data frame
(analysis4.df <- data.frame(floating.cash.flow, fixed.cash.flow, net.cash.flow, discount.factor, PV.of.net.cash.flow))

# Visualize it 
kable(analysis4.df, align = "c")
kable(Sum.PV.of.net.cash.flow)

#############
# Problem2 ##
#############

# The key principle to take away from this problem is that
# even though one party has an absolute advantage in borrowing 
# in to different markets relative to the other party in the swap,
# using its comparative advantage allows both parties to make savings
# and obtain funds at lower net borrowing costs.						

# Jaguar, a British manufacturer, wishes to borrow U.S. dollars at a fixed rate of interest.
# Ford, a U.S. multinational, wishes to borrow sterling at a fixed rate of interest.
# They have been quoted the following rates per annum (adjusted for differential tax effects):		

# Let put the values in a table
(Jaguar <- c(0.08, 0.05)) # Sterling, US$ borrowing rates per anum respectively 
(Ford <- c(0.072, 0.038)) # Sterling, US$ borrowing rates per anum respectively
(rates <- rbind(Jaguar, Ford))

# rename column names
c("GBP", "US$") -> colnames(rates)

# convert data frame
rates <- as.data.frame(rates)
#let's take a look at it
kable(rates)

## Design a swap that will net Citibank, a financial intermediary,
# 10 basis points (0.001) per annum .
# Make the swap equally attractive to the two companies and ensure that
# all foreign exchange risk is assumed by the bank.						

# Let's calculate the Differentials
(Differentials <- c(rates$GBP[1]-rates$GBP[2],rates$US[1]-rates$US[2]))

# Let's calculate the difference in Differentials
(difference.in.Differentials <- abs(Differentials[1] - Differentials[2]))

# Profit made by intermediary (Citibank)

(profit.made.by.Citibank <- 0.001)

# Savings made on borrowing costs (Jaguar)

(savings.made.on.borrowing.costs.Jaguar <- (difference.in.Differentials- profit.made.by.Citibank)/2)
# Savings made on borrowing costs (Ford)
(savings.made.on.borrowing.costs.Ford <- (difference.in.Differentials- profit.made.by.Citibank)/2)

print("Company with a comparative advantage at borrowing in U.S. dollars:FORD")

# Let's take a look at the borrowing rate Continuum for the companies
print("£: 8.00% <- Jaguar <- £: 8.00%, -> $: 4.85% Citibank <- £: 7.05%, -> $: 3.80% Ford  -> $: 3.80% ")




