# Adil Gokturk
# FIN 659

# HW8: TRADING STRATEGIES INVOLVING OPTIONS
# Textbook Reference:
# Section 12.3, pp. 256-264; 
# See also http://www.theoptionsguide.com/butterfly-spread.aspx,
# http://www.theoptionsguide.com/condor.aspx

# set working directory
setwd("~/Desktop/Spring2020/FIN659/Assignments/hw8")
getwd()

# Load the libraries
library(tidyverse)
library(quantmod)
library(optiRum)
library(jrvFinance)
library(knitr)
library(plotly)

options(scipen = 20) # adjust scientific numbers

###############
## Problem 1 ##
###############

# The key principle to take away from this problem is that
# trading strategies involving options can be constructed 
# for many different payoff profiles.						

# A condor (or condor spread) trading strategy is similar to 
# a butterfly spread – both can be executed with either calls or puts.						

# A long butterfly spread using calls involves three strike prices:
# two calls are sold at a middle strike,
# one call is bought above that strike and one call is bought below that strike.
# Thus, a bull call spread is combined with a bear call spread 
# with the short calls being at the same strike price.			

# A long condor spread also combines a bull call spread with a bear call spread,
# but separates the sold calls by at least one increment. 
# Condors have a wider range of profit, but cost more.	

# Both spreads are done for a debit 
# (meaning that there is an initial cost in setting up the strategy).	

# Consider an options trader who sets up a condor trading strategy on 
# Boston Scientific Corp. stock. 
# The option trader buys a call option with a strike price of $25,
# sells a call option with a strike price of $30,
# sells a call option with a strike price of $40,
# and buys a call option with a strike price of $45.		

## Complete the table below with the correct formulas 
# to show the profit/loss for different values of the stock price 
# at the expiration of the options.			

################
## 4 Formulas ##
################
# Call Options
(call.option <- rep(1:4))
(strike.price <- c(25, 30, 40, 45))
(option.price <- c(14.05, 9.70, 3.15, 1.38))


(stock.price <- rep(0:50))
max(0-25,0) - 14.05

# Calculate call option 1 for all stock prices
call.option1 <- rbind(max(stock.price[1] - strike.price[1],0) - option.price[1],
                       max(stock.price[2] - strike.price[1],0) - option.price[1],
                       max(stock.price[3] - strike.price[1],0) - option.price[1],
                       max(stock.price[4] - strike.price[1],0) - option.price[1],
                       max(stock.price[5] - strike.price[1],0) - option.price[1],
                       max(stock.price[6] - strike.price[1],0) - option.price[1],
                       max(stock.price[7] - strike.price[1],0) - option.price[1],
                       max(stock.price[8] - strike.price[1],0) - option.price[1],
                       max(stock.price[9] - strike.price[1],0) - option.price[1],
                       max(stock.price[10] - strike.price[1],0) - option.price[1],
                       max(stock.price[11] - strike.price[1],0) - option.price[1],
                       max(stock.price[12] - strike.price[1],0) - option.price[1],
                       max(stock.price[13] - strike.price[1],0) - option.price[1],
                       max(stock.price[14] - strike.price[1],0) - option.price[1],
                       max(stock.price[15] - strike.price[1],0) - option.price[1],
                       max(stock.price[16] - strike.price[1],0) - option.price[1],
                       max(stock.price[17] - strike.price[1],0) - option.price[1],
                       max(stock.price[18] - strike.price[1],0) - option.price[1],
                       max(stock.price[19] - strike.price[1],0) - option.price[1],
                       max(stock.price[20] - strike.price[1],0) - option.price[1],
                       max(stock.price[21] - strike.price[1],0) - option.price[1],
                       max(stock.price[22] - strike.price[1],0) - option.price[1],
                       max(stock.price[23] - strike.price[1],0) - option.price[1],
                       max(stock.price[24] - strike.price[1],0) - option.price[1],
                       max(stock.price[25] - strike.price[1],0) - option.price[1],
                       max(stock.price[26] - strike.price[1],0) - option.price[1],
                       max(stock.price[27] - strike.price[1],0) - option.price[1],
                       max(stock.price[28] - strike.price[1],0) - option.price[1],
                       max(stock.price[29] - strike.price[1],0) - option.price[1],
                       max(stock.price[30] - strike.price[1],0) - option.price[1],
                       max(stock.price[31] - strike.price[1],0) - option.price[1],
                       max(stock.price[32] - strike.price[1],0) - option.price[1],
                       max(stock.price[33] - strike.price[1],0) - option.price[1],
                       max(stock.price[34] - strike.price[1],0) - option.price[1],
                       max(stock.price[35] - strike.price[1],0) - option.price[1],
                       max(stock.price[36] - strike.price[1],0) - option.price[1],
                       max(stock.price[37] - strike.price[1],0) - option.price[1],
                       max(stock.price[38] - strike.price[1],0) - option.price[1],
                       max(stock.price[39] - strike.price[1],0) - option.price[1],
                       max(stock.price[40] - strike.price[1],0) - option.price[1],
                       max(stock.price[41] - strike.price[1],0) - option.price[1],
                       max(stock.price[42] - strike.price[1],0) - option.price[1],
                       max(stock.price[43] - strike.price[1],0) - option.price[1],
                       max(stock.price[44] - strike.price[1],0) - option.price[1],
                       max(stock.price[45] - strike.price[1],0) - option.price[1],
                       max(stock.price[46] - strike.price[1],0) - option.price[1],
                       max(stock.price[47] - strike.price[1],0) - option.price[1],
                       max(stock.price[48] - strike.price[1],0) - option.price[1],
                       max(stock.price[49] - strike.price[1],0) - option.price[1],
                       max(stock.price[50] - strike.price[1],0) - option.price[1],
                       max(stock.price[51] - strike.price[1],0) - option.price[1])

# Calculate call option 2 for all stock prices
call.option2 <- rbind(-(max(stock.price[1] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[2] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[3] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[4] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[5] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[6] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[7] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[8] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[9] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[10] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[11] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[12] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[13] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[14] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[15] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[16] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[17] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[18] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[19] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[20] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[21] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[22] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[23] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[24] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[25] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[26] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[27] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[28] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[29] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[30] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[31] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[32] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[33] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[34] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[35] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[36] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[37] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[38] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[39] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[40] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[41] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[42] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[43] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[44] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[45] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[46] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[47] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[48] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[49] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[50] - strike.price[2],0) - option.price[2]),
                       -(max(stock.price[51] - strike.price[2],0) - option.price[2]))

# Calculate call option 3 for all stock prices
call.option3 <- rbind(-(max(stock.price[1] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[2] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[3] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[4] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[5] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[6] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[7] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[8] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[9] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[10] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[11] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[12] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[13] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[14] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[15] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[16] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[17] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[18] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[19] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[20] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[21] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[22] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[23] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[24] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[25] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[26] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[27] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[28] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[29] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[30] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[31] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[32] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[33] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[34] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[35] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[36] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[37] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[38] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[39] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[40] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[41] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[42] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[43] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[44] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[45] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[46] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[47] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[48] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[49] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[50] - strike.price[3],0) - option.price[3]),
                       -(max(stock.price[51] - strike.price[3],0) - option.price[3]))


# Calculate call option 4 for all stock prices
call.option4 <- rbind(max(stock.price[1] - strike.price[4],0) - option.price[4],
                       max(stock.price[2] - strike.price[4],0) - option.price[4],
                       max(stock.price[3] - strike.price[4],0) - option.price[4],
                       max(stock.price[4] - strike.price[4],0) - option.price[4],
                       max(stock.price[5] - strike.price[4],0) - option.price[4],
                       max(stock.price[6] - strike.price[4],0) - option.price[4],
                       max(stock.price[7] - strike.price[4],0) - option.price[4],
                       max(stock.price[8] - strike.price[4],0) - option.price[4],
                       max(stock.price[9] - strike.price[4],0) - option.price[4],
                       max(stock.price[10] - strike.price[4],0) - option.price[4],
                       max(stock.price[11] - strike.price[4],0) - option.price[4],
                       max(stock.price[12] - strike.price[4],0) - option.price[4],
                       max(stock.price[13] - strike.price[4],0) - option.price[4],
                       max(stock.price[14] - strike.price[4],0) - option.price[4],
                       max(stock.price[15] - strike.price[4],0) - option.price[4],
                       max(stock.price[16] - strike.price[4],0) - option.price[4],
                       max(stock.price[17] - strike.price[4],0) - option.price[4],
                       max(stock.price[18] - strike.price[4],0) - option.price[4],
                       max(stock.price[19] - strike.price[4],0) - option.price[4],
                       max(stock.price[20] - strike.price[4],0) - option.price[4],
                       max(stock.price[21] - strike.price[4],0) - option.price[4],
                       max(stock.price[22] - strike.price[4],0) - option.price[4],
                       max(stock.price[23] - strike.price[4],0) - option.price[4],
                       max(stock.price[24] - strike.price[4],0) - option.price[4],
                       max(stock.price[25] - strike.price[4],0) - option.price[4],
                       max(stock.price[26] - strike.price[4],0) - option.price[4],
                       max(stock.price[27] - strike.price[4],0) - option.price[4],
                       max(stock.price[28] - strike.price[4],0) - option.price[4],
                       max(stock.price[29] - strike.price[4],0) - option.price[4],
                       max(stock.price[30] - strike.price[4],0) - option.price[4],
                       max(stock.price[31] - strike.price[4],0) - option.price[4],
                       max(stock.price[32] - strike.price[4],0) - option.price[4],
                       max(stock.price[33] - strike.price[4],0) - option.price[4],
                       max(stock.price[34] - strike.price[4],0) - option.price[4],
                       max(stock.price[35] - strike.price[4],0) - option.price[4],
                       max(stock.price[36] - strike.price[4],0) - option.price[4],
                       max(stock.price[37] - strike.price[4],0) - option.price[4],
                       max(stock.price[38] - strike.price[4],0) - option.price[4],
                       max(stock.price[39] - strike.price[4],0) - option.price[4],
                       max(stock.price[40] - strike.price[4],0) - option.price[4],
                       max(stock.price[41] - strike.price[4],0) - option.price[4],
                       max(stock.price[42] - strike.price[4],0) - option.price[4],
                       max(stock.price[43] - strike.price[4],0) - option.price[4],
                       max(stock.price[44] - strike.price[4],0) - option.price[4],
                       max(stock.price[45] - strike.price[4],0) - option.price[4],
                       max(stock.price[46] - strike.price[4],0) - option.price[4],
                       max(stock.price[47] - strike.price[4],0) - option.price[4],
                       max(stock.price[48] - strike.price[4],0) - option.price[4],
                       max(stock.price[49] - strike.price[4],0) - option.price[4],
                       max(stock.price[50] - strike.price[4],0) - option.price[4],
                       max(stock.price[51] - strike.price[4],0) - option.price[4])

# Let's put all option prices ina data frame
(all.options <- data.frame(call.option1,
                          call.option2,
                          call.option3,
                          call.option4))
# Caculate profit and loss
profit.loss <- rbind(sum(all.options[1,]),
                      sum(all.options[2,]),
                      sum(all.options[3,]),
                      sum(all.options[4,]),
                      sum(all.options[5,]),
                      sum(all.options[6,]),
                      sum(all.options[7,]),
                      sum(all.options[8,]),
                      sum(all.options[9,]),
                      sum(all.options[10,]),
                      sum(all.options[11,]),
                      sum(all.options[12,]),
                      sum(all.options[13,]),
                      sum(all.options[14,]),
                      sum(all.options[15,]),
                      sum(all.options[16,]),
                      sum(all.options[17,]),
                      sum(all.options[18,]),
                      sum(all.options[19,]),
                      sum(all.options[20,]),
                      sum(all.options[21,]),
                      sum(all.options[22,]),
                      sum(all.options[23,]),
                      sum(all.options[24,]),
                      sum(all.options[25,]),
                      sum(all.options[26,]),
                      sum(all.options[27,]),
                      sum(all.options[28,]),
                      sum(all.options[29,]),
                      sum(all.options[30,]),
                      sum(all.options[31,]),
                      sum(all.options[32,]),
                      sum(all.options[33,]),
                      sum(all.options[34,]),
                      sum(all.options[35,]),
                      sum(all.options[36,]),
                      sum(all.options[37,]),
                      sum(all.options[38,]),
                      sum(all.options[39,]),
                      sum(all.options[40,]),
                      sum(all.options[41,]),
                      sum(all.options[42,]),
                      sum(all.options[43,]),
                      sum(all.options[44,]),
                      sum(all.options[45,]),
                      sum(all.options[46,]),
                      sum(all.options[47,]),
                      sum(all.options[48,]),
                      sum(all.options[49,]),
                      sum(all.options[50,]),
                      sum(all.options[51,]))

# Let's put all the data in a data frame
all.options <- data.frame(stock.price,all.options, profit.loss)

# visualize it
kable(all.options,col.names = c("Stock Price $",
                                "Call Option1 $",
                                "Call Option2 $",
                                "Call Option3 $",
                                "Call Option4 $",
                                "Profit/Loss $"),  align = "c")
# Let's plot an interactive plot for 
# the  Profit/Loss Diagram for a Long Condor Spread
options.plot <- ggplot(data = all.options, aes(y = profit.loss, x = stock.price)) + geom_line(color="blue", size = 2) +
  geom_hline(yintercept = 0, lty = 2) + 
  xlab("Stock Price at time T") +
  ylab("Profit/Loss") +
  ggtitle("Profit/Loss Diagram for a Long Condor Spread")

#Static Plot
options.plot 

# interactive plot
ggplotly(options.plot)
