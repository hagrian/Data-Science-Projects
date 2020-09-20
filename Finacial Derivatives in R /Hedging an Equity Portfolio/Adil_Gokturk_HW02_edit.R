# Adil Gokturk
# FIN 659

# HW2 HEDGING AN EQUITY PORTFOLIO
# Textbook Reference:			Section 3.5, pp. 64-66	

## Problem						
# A fund manager has a portfolio worth $50 million with a beta of 0.87.
# The manager is concerned about the performance of the market over the next two months and
# plans to use three-month futures contracts on a well-diversified index to hedge its risk.
# The current index level is 2,670, one contract is on 250 times the index,
# the risk-free rate is 2.50% per annum, and the dividend yield on the index is 2%. 
# The current three-month futures price is 2,677.						

# set working directory
setwd("~/Desktop/Spring2020/FIN659/Assignments/hw2")
getwd()

# Load the libraries
library(tidyverse)

spot.index.value <- 2670
contract.index.multiplier <- 250
fund.value <- 50000000
fund.beta <- 0.87
(annual.risk.free.rate <- 2.50 ) # %
(annual.dividend.yield <- 2)  # %
three.month.futures.price <- 2677

## What position should the fund manager take to hedge exposure 
## to the market over the next two months?						

## Answer: Short Position

## Number of Future Contracts
contracts <- fund.beta*(fund.value/(contract.index.multiplier*three.month.futures.price))
contracts %>% round(0)

## Answer: We need 65 futures contract 

## Calculate the effect of your strategy on the fund manager's returns 
## if the level of the market in two months is 
## 2,500, 2,600, 2,700, and 2,800. 
## Assume that the one-month futures price is 0.25% higher than the index level at this time.

future.price.index.rate <- .0025 # Percentage that the futures price is higher than the index level

# Let's define the Value of index in two months		
(value.of.index.in.two.months <- c(2500, 2600, 2700, 2800))

# Calculate the Futures price of index in two months		
(futures.price.of.index.in.two.months <- (1+future.price.index.rate) * value.of.index.in.two.months)

# Calculate the Gain (loss) on futures position ($) and round zero
(gain.loss.on.future.positions.USD <-  65 * (three.month.futures.price - futures.price.of.index.in.two.months ) * contract.index.multiplier)
(gain.loss.on.future.positions.USD <- gain.loss.on.future.positions.USD %>% round(0))

# Calculate the Expected return on the index in two months (%),
# convert to % and round it to two decimal
(expected.return.on.the.index.in.two.months <- (value.of.index.in.two.months - spot.index.value)/spot.index.value)
(expected.return.on.the.index.in.two.months <- (expected.return.on.the.index.in.two.months *100) %>% round(2))

# Calculate the Expected return on the index (incl. dividends) in two months (%),
# convert to % and round it to two decimal
(expected.return.on.the.index.incl.div.in.two.months <- (expected.return.on.the.index.in.two.months + (annual.dividend.yield *2/12)))
(expected.return.on.the.index.incl.div.in.two.months <- expected.return.on.the.index.incl.div.in.two.months  %>% round(2))

# Calculate the Excess return on the index (incl. dividends) above risk-free rate (%)
(excess.return.on.the.index.incl.div.above.riskfree.rate <- (expected.return.on.the.index.incl.div.in.two.months-(annual.risk.free.rate * 2/12)) %>% round(2))

# Calculate the Excess return on (unhedged) portfolio above risk-free rate (%)
# Hint: Multiply the excess return on the index (including dividend yield) by the portfolio beta
(excess.return.on.unhedged.portfolio.above.riskfree.rate <- (excess.return.on.the.index.incl.div.above.riskfree.rate * fund.beta) %>% round(2))

# Calculate the Expected return on (unhedged) portfolio over two months (%)
# Hint: Excess return on the (unhedged) portfolio over two months plus the risk-free rate over two months
(expected.return.on.unhedged.portfolio.over.two.months <- (excess.return.on.unhedged.portfolio.above.riskfree.rate + (annual.risk.free.rate * 2/12)) %>% round(2))

# Calculate the Value of (unhedged) portfolio in two months ($)	
# Hint: Use the original value of the (unhedged) portfolio and the associated expected return over two months
(value.of.the.unhedged.porfolio.in.two.months.USD <- (fund.value %*% (1 + (expected.return.on.unhedged.portfolio.over.two.months/100))))

# Calculate the Total value of position in two months ($)		
(total.value.of.position.in.two.months_USD <- gain.loss.on.future.positions.USD + value.of.the.unhedged.porfolio.in.two.months.USD)

# Calculate the Expected return on (hedged) portfolio over two months (%)		
(expected.return.on.hedged.portfolio.in.two.months<- ((total.value.of.position.in.two.months_USD - fund.value)/fund.value))
(expected.return.on.hedged.portfolio.in.two.months<- (expected.return.on.hedged.portfolio.in.two.months*100) %>% round(2))

# Let's put it in a table
(analysis <- rbind(value.of.index.in.two.months,
                  futures.price.of.index.in.two.months,
                  gain.loss.on.future.positions.USD,
                  expected.return.on.the.index.in.two.months,
                  expected.return.on.the.index.incl.div.in.two.months,
                  excess.return.on.unhedged.portfolio.above.riskfree.rate,
                  expected.return.on.unhedged.portfolio.over.two.months,
                  value.of.the.unhedged.porfolio.in.two.months.USD,
                  total.value.of.position.in.two.months_USD,
                  expected.return.on.hedged.portfolio.in.two.months))

library(knitr)
kable(analysis)

row.names(analysis)

c("value.of.the.unhedged.porfolio.in.two.months.USD",
  "total.value.of.position.in.two.months_USD",
  "expected.return.on.hedged.portfolio.in.two.months") -> row.names(analysis)[8:10]













  