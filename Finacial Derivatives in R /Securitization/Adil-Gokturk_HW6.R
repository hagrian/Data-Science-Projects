# Adil Gokturk
# FIN 659

# HW6: SECURITIZATION

## Textbook Reference:		Section 8.1, pp. 184-188				


## Problem						
## The key principle to take away from this problem is that 
# there is an effect on securitized financial instruments 
# when borrowers default on their payments on 
#the underlying assets making up the securitized product.						

# A mortgage originator has recently decided to pool 2,000 of its mortgages into a portfolio.
# The average value of each mortgage in the pool is $250,000, and 
# the average interest rate is 4.80%. 
# The mortgage originator plans to sell the mortgages to an investment bank, 
#but it will continue to service the mortgages for a small fee (0.50% per year).						

# The cash flows from the assets will be allocated to tranches.
# The investment bank plans to create a mortgage-backed security with only three tranches:
# a senior tranche, 
# a mezzanine tranche, and 
# an equity tranche. 
# The principal from the portfolio of mortgages will be allocated as follows:
# 70% will go to the senior tranche (rated 'AAA' and earning 2.75%),
# 20% will go to the mezzanine tranche (rated 'A' and earning 3.75%), and 
# the remainder will go to the equity tranche (rated 'CCC'). 
# The investment bank plans to keep 0.80% per year as 
# its share for the administration of the securitization process.						

# set working directory
setwd("~/Desktop/Spring2020/FIN659/Assignments/hw6")
getwd()

# Load the libraries
library(tidyverse)
library(quantmod)
library(optiRum)
library(jrvFinance)
library(knitr)

# This information is summarized in the tables below.		

number.of.mortgages <- 2000
average.mortgage.value <- 250000 # US $
average.interest.rate <- 0.048 # %
mortgage.originator.fee <- 0.005 # % per year
investment.bank.fee <- 0.008 # % per year

senior.tranche <- c(0.7, 0.0275)
mezzanine.tranche <- c(0.2, 0.0375)
equity.tranche <- c(0.1, "NA")

(tranches <- rbind(senior.tranche, mezzanine.tranche, equity.tranche))
c("MBS Principal Allocation", "Rate") -> colnames(tranches)

# Let's take a look at the tranche data
kable(tranches)

## Question: What is the total value of the mortgage pool being sold to the investment bank?
options(scipen = 20) # convert scientific format penalize if is more than 20
(total.value.of.the.mortgage.pool <- number.of.mortgages * average.mortgage.value)

print("Answer: the total value of the mortgage pool = $500,000,000")

## What is the total annual interest expense paid by 
## the borrowers (those who took out mortgages to buy their homes)?		

(annual.interest.expense.paid.by.borrowers <- average.interest.rate * total.value.of.the.mortgage.pool)

print("Answer: the total annual interest expense paid by the borrowers = $24,000,000")


## From the payments obtained from the borrowers, 
# fees are paid to the mortgage originator and the investment bank, 
# and returns are provided to the MBS investors. 
# Complete the table below, 
# showing what the annual cash flow should be to each of the parties.						

# Mortgage originator
(mortgage.originator <- total.value.of.the.mortgage.pool * mortgage.originator.fee)
print("Answer: Mortgage originator  = $2,500,000")

# Investment bank
(investment.bank <- total.value.of.the.mortgage.pool * investment.bank.fee)
print("Answer: Investment bank = $4,000,000")

# Senior tranche investors
(senior.tranche.investors <- total.value.of.the.mortgage.pool %*% senior.tranche[1]* senior.tranche[2])
print("Answer: Senior tranche investors = $9,625,000")

# Mezzanine tranche investors
(mezzanine.tranche.investors <- total.value.of.the.mortgage.pool %*% mezzanine.tranche[1]  * mezzanine.tranche[2])
print("Answer: Mezzanine tranche investors = $3,750,000")

# Equity tranche investors
(equity.tranche.investors <- annual.interest.expense.paid.by.borrowers - mortgage.originator - investment.bank - senior.tranche.investors - mezzanine.tranche.investors)
print("Answer: Equity tranche investors = $4,125,000")

# let's put it in a table
table2 <- rbind(mortgage.originator,
                investment.bank,
                senior.tranche.investors,
                mezzanine.tranche.investors,
                equity.tranche.investors)
# Add row names
c("senior.tranche.investors",
  "mezzanine.tranche.investors",
  "equity.tranche.investors") -> row.names(table2)[3:5] # rename row names

# rename column names
c("US$") -> colnames(table2)
# better one
kable(table2, align = "c")

## Question: What is the MBS principal allocation to the equity tranche in dollar terms?	

# convert to numeric value
as.numeric(equity.tranche)-> equity.tranche
(MBS.principal.allocation.to.the.equity.tranche <- total.value.of.the.mortgage.pool * equity.tranche[1])

print("Answer: MBS principal allocation to the equity tranche in dollar = $50,000,000")

## Question: If the investment bank plans to keep the equity tranche for themselves,
## what annual return will be obtained on this investment?						
(annual.return.obtained.on.investment <- equity.tranche.investors/MBS.principal.allocation.to.the.equity.tranche)

print("Answer: Annual return obtained on this investment = 8.25%")


## PART II:
# The investment bank plans to create a collateralized mortgage obligation (CMO)
# from the mezzanine tranche of the MBS.
# The principal assigned to the senior, mezzanine, and equity tranches of the CMO 
# differ from that assigned to the MBS; 
# the credit ratings and coupon rates for the CMO are the same as for the MBS.
# The principal from the mezzanine tranche of the MBS will be allocated as follows:
# 65% will go to the senior tranche (rated ‘AAA’ and earning 2.75%),
# 25% will go to the mezzanine tranche (rated ‘A’ and earning 3.75%), and 
# the remainder will go to the equity tranche (rated ‘CCC’). 
# All of this information is represented in the diagram and table below.						


senior.tranche <- c(0.65, 0.0275)
mezzanine.tranche <- c(0.25, 0.0375)
equity.tranche <- c(0.1, "NA")

#equity.tranche <- as.numeric(equity.tranche)
# let's put it in a table
(tranches2 <- rbind(senior.tranche, mezzanine.tranche, equity.tranche))
tranches2 <- as.data.frame(tranches2)
# Rename column names
c("CMO Principal Allocation", "Rate") -> colnames(tranches2)

# Let's take a look at the tranche data
kable(tranches2, align = "c")

# Suppose that the economy experiences a real estate crisis,
# prices drop, and many of the original borrowers decide to default on their mortgages
# and abandon their homes. 
# Complete the table below to show the estimated losses to the three tranches of the CMO.						

# Losses on underlying assets
(losses.on.underlying.assets <- c(0.1, 0.125, 0.15, 0.175, 0.2))
as.numeric(equity.tranche)
# Losses to mezzanine tranche of MBS
# mezzanine tranche of MBS = 0.2
(equity.tranche <- 0.1)

(losses.to.mezzanine.tranche.of.MBS <- ifelse(test = losses.on.underlying.assets <= equity.tranche,
                                              yes = 0,
                                              no = (losses.on.underlying.assets - equity.tranche) / 0.2 ))

# Losses to equity tranche of CMO
# mezzanine tranche of MBS = 0.10
(losses.to.equity.tranche.of.CMO <- ifelse(test = losses.to.mezzanine.tranche.of.MBS < 0.10,
                                           yes = equity.tranche / losses.to.mezzanine.tranche.of.MBS,
                                           no = 1 ))

# Losses to mezzanine tranche of CMO
# Equity tranche = 0.10
# Mezzanine tranche = 0.25
(losses.to.mezzanine.tranche.of.CMO <- ifelse(test = losses.to.mezzanine.tranche.of.MBS< equity.tranche, yes = 0,
                                              no = ifelse(test = losses.to.mezzanine.tranche.of.MBS < (mezzanine.tranche[1]  + equity.tranche),
                                                          yes = (losses.to.mezzanine.tranche.of.MBS - equity.tranche)/ mezzanine.tranche[1],
                                                          no = 1 )))

# Losses to senior tranche of CMO
# Equity tranche = 0.10
# Mezzanine tranche = 0.25
# Senior tranche = 0.65
(losses.to.senior.tranche.of.CMO <- ifelse(test =  losses.to.mezzanine.tranche.of.MBS < mezzanine.tranche[1]  + equity.tranche, 
                                           yes = 0,
                                           no = (losses.to.mezzanine.tranche.of.MBS- mezzanine.tranche[1]  - equity.tranche)/senior.tranche[1]))

# Let's put all in a table
est.losses.of.all.tranches <- data.frame(losses.on.underlying.assets, 
                                         losses.to.mezzanine.tranche.of.MBS,
                                         losses.to.equity.tranche.of.CMO,
                                         losses.to.mezzanine.tranche.of.CMO,
                                         losses.to.senior.tranche.of.CMO)

# rename column names
c("Underlying Assets",
  "MBS Mezz. Tranche",
  "CMO Equity Tranche",
  "CMO Mezz. Tranche",
  "CMO Senior Tranche") -> colnames(est.losses.of.all.tranches)

# Visualize it 
kable(est.losses.of.all.tranches, align = "c")




















