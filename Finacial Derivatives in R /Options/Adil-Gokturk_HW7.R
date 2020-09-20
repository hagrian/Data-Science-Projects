# Adil Gokturk
# FIN 659

# HW7: BASICS OF OPTIONS
# set working directory
setwd("~/Desktop/Spring2020/FIN659/Assignments/hw7")
getwd()

# Load the libraries
library(tidyverse)
library(quantmod)
library(optiRum)
library(jrvFinance)
library(knitr)

options(scipen = 20) # adjust scientific numbers

# Textbook Reference:Section 1.8, pp. 14-16;
# Section 10.4, pp. 215-219; Section 10.7, pp. 221-222;
# Section 11.4, pp. 238-241, Equation 11.10

###############
## Problem 1 ##
###############
# The key principle to take away from this problem is 
# that leverage can have significant effects when it comes to investing.
# Leverage allows relatively small amounts of capital 
# to generate large profits - a magnification effect.
# The danger of leverage is that losses are also magnified.
# Options contracts are a leverage tool 
# as they allow investors to multiply the power of their starting capital.						

# The current price of DISH Network stock is $31.50 per share,
# and six-month European call options on the stock with a strike price of $32.50 
# are currently trading at $3.60. 
# An investor, who has $10,000 of capital to invest,
# believes that the price of the stock will increase by 20% over the next six months.
# The investor is trying to decide between two strategies - 
# (A) buying shares or 
# (B) buying call options.
# What return will each strategy produce after six months,
# if the investor is correct in their assessment of the stock?
# Assume that either a whole number of shares can be bought OR 
# a whole number of option contracts cane be bought 
# (representing the right to buy 100 shares per option contract).		


# This information is summarized in the tables below.		

# Hint: The price for the call option is for an option on 1 share;
# each option contract is for options on 100 shares.

# Price of security
(price.of.security <- c(31.50, 3.60)) # US$

# Strike price of the call options
(strike.price.of.the.call.options <- c("NA", 32.50)) # US$

# let's put it in a table and convert to df
(df1 <- rbind(price.of.security, strike.price.of.the.call.options))
(df1 <- as.data.frame(df1))


# rename column names
c("Strategy A:Buy shares",
  "Strategy B:Buy call options") -> colnames(df1)
kable(df1, align = "c")
# Amount available for investment
(amount.available.for.investment <- 10000) # US$
# Anticipated change in stock price
(Anticipated.change.in.stock.price <- 0.2) 


## How many shares can be purchased under Strategy A?
## How many option contracts can be purchased under Strategy B? 
# (Round your answers to the nearest whole number.)						

# Strategy A
(Strategy.A <- amount.available.for.investment/price.of.security[1])
(Strategy.A <- floor(Strategy.A) ) # nearest integer
print("Strategy A: 317 shares can be purchased")

# Strategy B
(Strategy.B <- (amount.available.for.investment/(100*price.of.security[2])))
(Strategy.B <- floor(Strategy.B))# nearest integer
print("Strategy A: 27 options contracts can be purchased")

## How much uninvested cash does the investor still have?
# (Assume that this cash remains in a non-interest-bearing account.)						

# Strategy A
(Strategy.A <- amount.available.for.investment - (price.of.security[1] * Strategy.A))
print("Strategy A: $14.50")

# Strategy B
(Strategy.B <- amount.available.for.investment - (Strategy.B * 100 * price.of.security[2]))
print("Strategy B: $280.00")

## Assume that the anticipated change in stock price is
## realized over the next six months. 
## What is the price of a share of the stock?	
(price.of.security[1] * (1 + Anticipated.change.in.stock.price))
print("The price of a share of the stock is $37.80")

## What is the value of all of the shares purchased 
## if the anticipated change in stock price is realized?
## What is the intrinsic value of the options purchased,
## assuming they are all exercised at maturity 
## if the anticipated change in stock price is realized?		

# Strategy A
(Strategy.A <- floor(amount.available.for.investment/price.of.security[1])*
    (price.of.security[1] * (1 + Anticipated.change.in.stock.price)))
print("Strategy A: the intrinsic value of the options = $11,982.6")

# Strategy B
(Strategy.B <- (max(37.80-32.50,0)*27*100))
print("Strategy B: $14,310.00")

## What is the total profit made in dollar terms?
# Hint: Remember to add back the uninvested cash 
# before subtracting the initial investment

# Strategy A
(Strategy.A <-11982.60 + 14.50 - amount.available.for.investment) 
print("Strategy A: $1,997.10")

# Strategy B
(Strategy.B <- 14310 + 280 - amount.available.for.investment)
print("Strategy B: $4,590.00")

## What is the total profit made (in percentage terms)?
##In other words, express the answers above 
## as a percentage of the initial capital available for investment.						

# Strategy A
(Strategy.A <-1997.10/amount.available.for.investment*100)
print("Strategy A: 19.97%")

# Strategy B
(Strategy.B <- (14310 + 280 - amount.available.for.investment)/amount.available.for.investment * 100)
print("Strategy B: 45.90%")

## Now consider the outcome to the two strategies 
## if the stock price were to decrease by 20%.						

# Anticipated change in stock price 20%

(anticipated.change.in.stock.price <- 0.20)

## What is the price of a share of the stock in this second scenario?	
(price.of.security[1] * (1 + anticipated.change.in.stock.price))
print("The price of a share of the stock in this second scenario = $37.80")

## What is the value of all of the shares purchased 
## if the anticipated change in stock price is realized?
## What is the intrinsic value of the options purchased,
## assuming they are all exercised at maturity 
## if the anticipated change in stock price is realized?					

# Strategy A
(Strategy.A <-25.20*317)
print("Strategy A: $7,988.40")


# strike.price.of.the.call.options[2]
# Strategy B
(Strategy.B <- max(25.20 - 32.50,0)* 27 *100)
print("Strategy B: $0.00")

## What is the total loss made in dollar terms?						

# Strategy A
(Strategy.A <-7988.4 + 14.50 - amount.available.for.investment)
print("Strategy A: -$1,997.10")

# Strategy B
(Strategy.B <- (max(25.20 - 32.50,0)* 27 *100)+280 - amount.available.for.investment)
print("Strategy B: -$9,720.00")

## What is the total profit made (in percentage terms)? 
## In other words, express the answers above 
## as a percentage of the initial capital available for investment.						
# Strategy A
(Strategy.A <-(7988.4 + 14.50 - amount.available.for.investment)/amount.available.for.investment *100)
round(Strategy.A,2)
print("Strategy A: -19.97%")

# Strategy B
(Strategy.B <- ((max(25.20 - 32.50,0)* 27 *100)+280 - amount.available.for.investment)/
    amount.available.for.investment *100)
print("Strategy B: -97.20%")

## Which strategy has a higher standard deviation of returns?
## In other words, which investment strategy is riskier?		
print("Strategy B: Buying call option has  a higher standard deviation of returns")

###############
## Problem 2 ##
###############

## The main concept to remember from this question is that
## an exchange will alter the terms of options contracts 
## if a company decides to significantly change the number of shares it has outstanding.
## Assuming the market capitalization of the company remains constant,
## an increase in the number of shares outstanding 
## will automatically result in a decrease in the price per share.
## The terms of an options contract usually do not change when a cash dividend is 
## paid by the company (although a large special dividend would be an exception to this).	

## Consider a call option to buy 100 shares of a company for $60 per share.
## How do the terms of the option contract change in each of the following cases?
## (If there is no change in the terms, write in the original terms of the contract.)						

# Original terms of the option contract:
(original.terms.of.the.option.contract <- c(100, 60))

# A cash dividend:		Dividend amount		$5.00
(cash.divident <- c(original.terms.of.the.option.contract[1], original.terms.of.the.option.contract[2]))

# A stock split:		Split ratio		4/1
(split.ratio <- 4/1)
(stok.split <- c(original.terms.of.the.option.contract[1]* split.ratio, original.terms.of.the.option.contract[2]/split.ratio))

# A stock dividend:		Dividend percentage		10%
(divident.percentage <- 0.1)
(stock.divident <- c(original.terms.of.the.option.contract[1]*(1+divident.percentage), 
                     original.terms.of.the.option.contract[2]/(1+divident.percentage)))
round(stock.divident,2)

# lets put it in a table

(df2 <- rbind(original.terms.of.the.option.contract,
              cash.divident,
              stok.split,
              stock.divident))
(df2 <- round(df2, 2))              

# # rename column names
c("No. of shares to buy",
  "Strike price $") -> colnames(df2)

# Visualize it 
kable(df2, align = "c")

###############
## Problem 3 ##
###############

## The central idea to recognize in this example is that 
## writing options (that is, selling them) involves risk for the party
## that is doing the selling. 
## An exchange will require that margin be put up by the option writer 
## to mitigate the possibility that they will default on a loss-making position.
## An investor buying an option does not have a margin requirement,
## as the maximum loss is the cost of the option and this is paid up front.			

## A United States investor writes eight naked call option contracts and
## eleven naked put option contracts (each contract is for options on 100 shares).
## The call option price is $5.00,
## the put option price is $10.55,
## the strike price for both calls and puts is $90.00,
## and the stock price is $84.00.
## What is the initial margin requirement for the investor?						

# This information is summarized in the table below.						

# No. of call option contracts sold			8
(number.of.call.option.contracts.sold <- 8)

# Call option price			$5.00
(call.option.price <- 5.00)

# No. of put option contracts sold			11
(number.of.put.option.contracts.sold <- 11)

# Put option price			$10.55
(put.option.price <- 10.55)

# Strike price			$90.00
(strike.price <- 90.00)

# Stock price			$84.00
(stock.price <- 84.00	)

# let's put it in a df
(df3 <- rbind(number.of.call.option.contracts.sold,
          call.option.price,
          number.of.put.option.contracts.sold,
          put.option.price,
          strike.price,
          stock.price))
# rename column name
c("Values") -> colnames(df3)

# Visualize it 
kable(df3, align = "c")

## The initial and maintenance margin for a written naked call option
# is the greater of the following two calculations:						

# Calculation 1: 
# A total of 100% of the proceeds of 
# the sale plus 20% of the underlying share price 
# less the amount if any by which the option is out of the money		

## Margin requirement for all written call option contracts

# Hint: Remember to multiply the margin requirement for one option
# by the number of option contracts times 100
(number.of.call.option.contracts.sold*100*(call.option.price + 0.2* 
                                             stock.price -(strike.price - stock.price)))

print("Margin requirement for all written call option contracts = $12,640.00")

# Calculation 2:
# A total of 100% of the option proceeds plus 10% of the underlying share price	
## Margin requirement for all written call option contracts

# Hint: Remember to multiply the margin requirement for one option 
#by the number of option contracts times 100
(number.of.call.option.contracts.sold * 100 *
    (call.option.price+ 0.1 * stock.price))

print("Margin requirement for all written call option contracts = $10,720.00")

## Therefore, the initial and maintenance margin for all written naked call options is:						
(max(12640, 10720))

print("Therefore, the initial and maintenance margin for all written naked call options is: $12,640")

## The initial and maintenance margin for a written naked put option is the greater of:						

# Calculation 1:
# A total of 100% of the proceeds of the sale 
# plus 20% of the underlying share price 
# less the amount if any by which the option is out of the money						

## Margin requirement for all written put option contracts
# Hint: Remember to multiply the margin requirement for 
# one option by the number of option contracts times 100
(number.of.put.option.contracts.sold*100*
    (put.option.price+0.2 * stock.price))

print("Margin requirement for all written PUT option contracts = $30,085.00")

# Calculation 2:
# A total of 100% of the option proceeds plus 10% of the exercise price						
# Margin requirement for all written put option contracts
# Hint: Remember to multiply the margin requirement for
# one option by the number of option contracts times 100
(number.of.put.option.contracts.sold*100*
    (put.option.price+0.1*strike.price))

print("Margin requirement for all written PUT option contracts = $21,505.00")

## Therefore, the initial and maintenance margin for all written naked call options is:						
(max(30085, 21505))

print("Therefore, the initial and maintenance margin for all written naked put options is: $30,085")

## Hence, the total initial margin requirement for the investor is:
(total.initial.margin.requirement <- 30085 + 12640)
print("Hence, the total initial margin requirement for the investor is: $42,725")

###############
## Problem 4 ##
###############

# The key principle to take away from this problem is that
# arbitrage opportunities arise when securities are not trading at their theoretical prices.
# If security A is overvalued relative to security B,
# then security A should be sold and security B should be bought ("buy low, sell high").
# As more and more market participants observe the arbitrage opportunity,
# selling pressure will cause the price of security A to decrease and
# buying pressure will cause the price of security B to
# increase until an equilibrium is reached where the arbitrage opportunity no longer exists.
# (Note that selling a zero-coupon bond is equivalent to borrowing money,
# and buying a zero-coupon bond is equivalent to investing.)		

## A European call option and put option on a stock both have a strike price of $20 and
## an expiration date in three months.
## Both sell for $3. 
## The risk-free interest rate is 10% per annum,
## the current stock price is $19, 
## and a $1 dividend is expected in one month.
## What is the arbitrage opportunity open to a trader?		

# This information is summarized in the table below.	

# Price of European call option			$3.00
(price.of.European.call.option <- 3)

# Price of European put option			$3.00
(price.of.European.put.option <- 3)

# Strike price of both options			$20.00
(strike.price <- 20)
# Time to expiration (months)			3
(time.to.expiration <- 3)

# Risk-free interest rate			10%
(risk.free.rate <- 0.1)

# Current stock price			$19.00
(stock.price <- 19)

# Expected dividend			$1.00
(expected.dividend <- 1)

# Time until dividend received (months)			1
(time.until.dividend.received <- 1)

# let's put it in a table
(df4 <- rbind(price.of.European.call.option,
              price.of.European.put.option,
              strike.price,
              time.to.expiration,
              risk.free.rate,
              stock.price,
              expected.dividend,
              time.until.dividend.received))

# rename column name
c("Values") -> colnames(df4)

# Visualize it 
kable(df4, align = "c")

## What is the present value of the expected dividend?						
(expected.dividend * exp(-risk.free.rate * (time.until.dividend.received/12)))

print("the present value of the expected dividend is: $0.99")

## What is the value of the Left-Hand Side (LHS) of 
# the put-call parity equation above?		
(price.of.European.call.option + 0.99 +
    (strike.price * exp(-risk.free.rate * 
                          (time.to.expiration/12)))) %>% round(2)

print("The value of the Left-Hand Side (LHS) of the put-call parity is: $23.50")

## What is the value of the Right-Hand Side (RHS) of the put-call parity equation above?
(price.of.European.put.option + stock.price)

print("The value of the Right-Hand Side (RHS) of the put-call parity is $22.00")

## Which side of the put-call parity equation is overvalued (and should therefore be "sold")?	

print("the Left-Hand Side (LHS) of the put-call parity equation is overvalued")

## Payoff Table for the Arbitrage Opportunity
## Hint: Refer to Table 10.3 in the textbook

(sell.a.call.option <- c(price.of.European.call.option, "", "(S-$20.00)", "$0.00"))
(sell.a.bond.FV.D.maturity <- c("$0.99","-$1.00","",""))
(sell.a.bond.FV.K.maturity <- c(round(strike.price *exp(-risk.free.rate * (time.to.expiration/12)),2),
                                                                     "","-$20.00", "-$20.00"))
(buy.a.put.option <- c(-(price.of.European.put.option), "", "$0.00", "($20.00-S)"))
(buy.a.share.of.stock <- c(-(stock.price), "","Sell Stock", "Sell Stock"))
(receive.the.divident.in.1.month <- c("", expected.dividend,"",""))
(Totals <- c("$1.50", "$0.00", "$0.00", "$0.00"))

# let's put it in a table
(df5 <- rbind(sell.a.call.option,
              sell.a.bond.FV.D.maturity,
              sell.a.bond.FV.K.maturity,
              buy.a.put.option,
              buy.a.share.of.stock,
              receive.the.divident.in.1.month,
              Totals))

# rename column name
c("T = 0",
  "T = 1/12",
  "ST ≥ K",
  "ST < K") -> colnames(df5)

# Visualize it 
kable(df5, align = "c")


