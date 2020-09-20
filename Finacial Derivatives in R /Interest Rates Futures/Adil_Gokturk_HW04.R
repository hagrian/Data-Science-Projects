# Adil Gokturk
# FIN 659
# HW4 INTEREST RATE FUTURES

# Textbook Reference:			Section 6.2, pp. 138-143; Section 6.3, pp. 143-148; Section 6.4, pp. 148-152

# Proplem 1
# The December Eurodollar futures contract is quoted as 98.40 and 
# a company plans to borrow $8 million for three months starting in December at LIBOR plus 0.5%.	

# Working Directory
setwd("~/Desktop/Spring2020/FIN659/Assignments/hw4") # set working directory
getwd() # check it


Quoted.futures.price=98.40
Time.to.maturity.of.contract=3 #  (months)			
Principal=8000000#US$
Spread.over.LIBOR=0.5 # %
Euro.Dollar.Contact.Value=1000000
Actual.three.month.rate.at.settlement=0.013 #1.3%
## Question: What position should the company take in the contracts?
  ## Answer: SHORT position

## Question: How many contracts they need to hedge their position?
(contracts <- Principal/Euro.Dollar.Contact.Value)
  ## Answer:8 contracts

## Question: What rate can the company lock in by using the Eurodollar futures contract?
round((Euro.dollar.rate <- ((100-Quoted.futures.price)+Spread.over.LIBOR)/100*100),2) # %
  ## Answer:2.1 % 

## Question: If the actual three-month rate turns out to be 1.3%, 
## what is the final settlement price on the futures contracts?	

(Final.settlement.proce <- 100 - (100*Actual.three.month.rate.at.settlement))

  ## Final settlement price is US$98.70

###############
# Proplem 2
## On August 1, a portfolio manager has a bond portfolio worth $50 million. 
## The duration of the portfolio in October will be 7.1 years. 
## The December Treasury bond futures price is currently 91-12 and 
## the cheapest-to-deliver bond will have a duration of 8.8 years at maturity.						

Value.of.bond.portfolio <- 50000000 # US$
Duration.of.the.portfolio <- 7.1 #  years
Duration.of.cheapest.to.deliver.bond <- 8.8 #  years
Face.value.of.a.Treasury.bond.futures <- 100000 # US$

## Question: What is the December Treasury bond futures price as a decimal?


(December.Treasury.bond.futures.price <- 91+(12/32)) #US$
  ## Answer:the December Treasury bond futures price = US$91.375


## Question: What is the value of one December Treasury bond futures contract?
(December.Treasury.bond.futures.contract <- December.Treasury.bond.futures.price/100*Face.value.of.a.Treasury.bond.futures)

  ## Answer:  The value of one December Treasury bond futures contract is $91375

## Question: What position should the portfolio manager use to immunize 
## the portfolio against changes in interest rates over the next two months?		

  ## Answer: We own the asset. Position for the future contracts should be SHORT

## Question: How many contracts they need to hedge their position?

(contracts2 <- (Duration.of.the.portfolio*Value.of.bond.portfolio)/(Duration.of.cheapest.to.deliver.bond*December.Treasury.bond.futures.contract))
round(contracts2,0)

  ## Answer:441 contracts


New.duration.of.the.portfolio <- 10.0 #  years	

## Question: What position should the portfolio manager use to change the duration from 
## its original duration of 7.1 years to 
## a new duration of 10.0 years over the next two months?				

  ## Answer:We should take a LONG position in order to increase the duration of the portfolio 


## Question: How many contracts they need to hedge their new position?

(contacts3 <- ((New.duration.of.the.portfolio - Duration.of.the.portfolio)*Value.of.bond.portfolio)/(Duration.of.cheapest.to.deliver.bond*December.Treasury.bond.futures.contract))
round(contacts3,0)

  ## ## Answer:180 contracts





