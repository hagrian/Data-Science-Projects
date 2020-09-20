
setwd("~/Desktop/Spring2020/FIN654/Weeks/w9")
getwd()

head(myxts)
tail(myxts)
library(fPortfolio)
library(quantmod)

# FIRST Load 12Stocks10.data file 
# convert xts to timeseries object
mytimeseries <- as.timeSeries(myxts)
head(mytimeseries) # GMT 

# Rady to portfolio optimization
minrisk <- minvariancePortfolio(mytimeseries)
minrisk # portfolio weigths to min to risk and volatibilty
# cov means min std dev(risk) of the minimum risk portfolio

# Optimizing portfolio
# Sharp ratio (reward-to-volatilty is maximized)
# annual risk free rate approx= 2.0%
# montly => 0.02/12= 0.0017 montly (FED rezerve annualized treasury bills/bonds)

rf <- portfolioSpec() # defien rf as a portfolio spec function
0.0017 -> setRiskFreeRate(rf)
rf

maxSharpe <- tangencyPortfolio(data = mytimeseries, spec = rf)
maxSharpe # maximazed risk adjusted porfolio
 # sharpe ratio(risk premium, maximized/optimized predicted return) = expected(predicted) return - riskFree Return

# cov(risk free return or std dev) - target mean (expected return)
(0.0137 -0.0017)/0.0327 # sharpe ratio calculation

# sharpe ratio 0.367 (maxized std dev and risk value  or reward-to-volatilty is maximized)

# Plot the Efficient Frontier 
# border of "all" possible portfolios
myfrontier <- portfolioFrontier(data = mytimeseries, spec = rf)
plot(myfrontier)


# export data from frontier

mypoints <- frontierPoints(myfrontier)
tail(mypoints) # starts from bottom to top (1 to 50)

# get the weight of the portfolio


myweights <- getWeights(object = myfrontier)
tail(myweights)
options(scipen = 20) # convert scientific format penalize if is more than 20

# let's combine the objects by using cbind
myexport <- cbind(mypoints, myweights)
write.csv(myexport, file = "myexport.csv")


# to increase number of points to 200 
200 ->setNFrontierPoints(rf) 
rf

# rerunthe frontier
# Plot the Efficient Frontier 
# border of "all" possible portfolios
myfrontier <- portfolioFrontier(data = mytimeseries, spec = rf)
plot(myfrontier)


# suppoe short sellin
# solverquadprog cannot handle short selling
# set the solver type
# solveRshortExact
"solveRshortExact" -> setSolver(rf)
rf

# recompute to maximize tangeny Portfolio and allow short selling
shortSharpe <- tangencyPortfolio(data = mytimeseries, spec = rf, constraints = "short") # allow short selling
shortSharpe # check negative weight which are short selling ( HILTON = -0.2963 etc)

# RE-WATCH VIDEO!!!!!!!!!!

#########################
## Week 10 – Aggregating Enterprise Risks
# Unit 6 | Enterprise Risk Analytics
# singular value decomposition
library(mvtnorm)
set.seed(1016)
n.risks <- 3 # Number of risk factors : Revenue, Fix and Variable costs
m <- n.risks
n.sim <- 1000
sigma <- matrix(c(1, 0.4, 0.2, 0.4, 1, -0.8, 0.2, -0.8, 1), nrow = 3)
z <- rmvnorm(n.sim, mean = rep(0, nrow(sigma)), sigma = sigma, method = "svd")

library(psych)
cor(z, method = "spearman") # Textbook calculation
cor(z, method = "pearson") # Rank order calculation
pairs.panels(z)

# Exercise: Skylar's in the House
u <- pnorm(z)
pairs.panels(u)

# uniform Distribution 

library(rgl)
plot3d(u[, 1], u[, 2], u[, 3], pch = 20, col = "orange")

# Starting from a multivariate normal distribution we created dependent uniform
# variates. Using the dependent uniform variates we created dependent
# distributions of our choosing.
x1 <- qgamma(u[, 1], shape = 2, scale = 1)
x2 <- qbeta(u[, 2], 2, 2)
x3 <- qt(u[, 3], df = 5)

factors.df <- cbind(x1/10, x2, x3/10)
colnames(factors.df) <- c("Revenue",
                          "Variable Cost", "Fixed Cost")
pairs.panels(factors.df)
cor(factors.df, method = "spearman")

# Analyze

revenue <- 1000 * (1 + factors.df[, 1])
variable.cost <- revenue * factors.df[,
                                      2]
fixed.cost <- revenue * factors.df[,
                                   3]
total.cost <- variable.cost + fixed.cost
operating.margin <- revenue - variable.cost -
  fixed.cost
analysis <- cbind(revenue, total.cost,
                  operating.margin)
colnames(analysis) <- c("Revenue", "Cost",
                        "Margin")
#Here’s the result.
pairs.panels(analysis)

# Exercise: Risk Measures
### Simple Value at Risk
expected.margin <- 400
## Center margin loss on expected
## margin
loss.rf <- -(expected.margin - operating.margin)
## Assign metric of interest to
## reusable code
summary(loss.rf)

## Always review a key variable's
## content
alpha.tolerance <- 0.99
## Very intolerant! Remember that
## putting a variable assignment in
## parentheses also prints the result
(VaR.hat <- quantile(loss.rf, probs = alpha.tolerance,
                     names = FALSE))

### Just as simple Expected shortfall
(ES.hat <- mean(loss.rf[loss.rf > VaR.hat]))

# Let’s plot the results.
hist(loss.rf, xlab = "Operating Margin",
     ylab = "Frequency", main = "Margin Loss Tolerance")
abline(v = VaR.hat, col = "red")

# Example
set.seed(1016) ## Freezes the random seed to reproduce results exactly
n.risks <- 3 ## Number of risk factors
m <- n.risks
n.sim <- 1000
sigma <- matrix(c(1, 0.4, 0.2, 0.4, 1,
                  -0.8, 0.2, -0.8, 1), nrow = m)
z <- rmvt(n.sim, delta = rep(0, nrow(sigma)),
          sigma = sigma, df = 6, type = "shifted")

pairs.panels(z)

u <- pt(z, df = 6)
pairs.panels(u)

x1 <- qgamma(u[, 1], shape = 2, scale = 1)
x2 <- qbeta(u[, 2], 2, 2)
x3 <- qt(u[, 3], df = 6)

# Next we combine the series into a data frame and review the scatterplot matrix.
factors.df <- cbind(x1/10, x2, x3/10)
colnames(factors.df) <- c("Revenue",
                          "Variable Cost", "Fixed Cost")
pairs.panels(factors.df)

# cor(df,meth='spearman') could also 
# be run to verify the pairs.panels()

revenue <- 1000 * (1 + factors.df[, 1])
variable.cost <- revenue * factors.df[,
                                      2]
fixed.cost <- revenue * factors.df[,
                                   3]
total.cost <- variable.cost + fixed.cost
operating.margin <- revenue - variable.cost -
  fixed.cost
analysis.t <- cbind(revenue, total.cost,
                    operating.margin)
colnames(analysis.t) <- c("Revenue",
                          "Cost", "Margin")

# Scatterplot


pairs.panels(analysis.t)

## Let’s build an app

# The simulation function
library(shiny)
require(mvtnorm)
require(psych)
risk.sim <- function(input) {
  # Begin enterprise risk simulation
  set.seed(1016) # Freezes the random seed to reproduce results exactly
  n.risks <- 3 # Number of risk factors
  m <- n.risks
  n.sim <- 1000 # pull slider settings into the sigma correlation matrix
  sigma <- matrix(c(1, input[1], input[2], input[1], 1, input[3], input[2],
                    input[3], 1), nrow = m)
  z <- rmvt(n.sim, delta = rep(0, nrow(sigma)), sigma = sigma, df = 6, type = "shifted")
  u <- pt(z, df = 6)
  x1 <- qgamma(u[, 1], shape = 2, scale = 1)
  x2 <- qbeta(u[, 2], 2, 2)
  x3 <- qt(u[, 3], df = 6)
  factors.df <- cbind(x1/10, x2, x3/10)
  colnames(factors.df) <- c("Revenue", "Variable Cost", "Fixed Cost")
  revenue <- 1000 * (1 + factors.df[, 1])
  variable.cost <- revenue * factors.df[, 2]
  fixed.cost <- revenue * factors.df[, 3]
  total.cost <- variable.cost + fixed.cost
  operating.margin <- revenue - variable.cost - fixed.cost
  analysis.t <- cbind(revenue, total.cost, operating.margin)
  colnames(analysis.t) <- c("Revenue", "Cost", "Margin")
  return(analysis.t)
}

# Here is what the Shiny UI code looks like:
ui <- fluidPage(titlePanel("Enterprise Risk Analytics"),
                sidebarLayout(sidebarPanel(sliderInput(inputId = "cor.1",
                                                       label = "Set the Revenue - Variable Cost Correlation",
                                                       value = 0, min = -0.9, max = 0.9),
                                           sliderInput(inputId = "cor.2",
                                                       label = "Set the Revenue - Variable Cost Correlation",
                                                       value = 0, min = -0.9, max = 0.9),
                                           sliderInput(inputId = "cor.3",
                                                       label = "Set the Variable - Fixed Cost Correlation",
                                                       value = 0, min = -0.9, max = 0.9)),
                              mainPanel(plotOutput("pairs.1"))))


# The server
server <- function(input, output) {
  output$pairs.1 <- renderPlot({
    analysis.t <- risk.sim(c(input$cor.1,
                             input$cor.2, input$cor.3))
    pairs.panels(analysis.t)
  })
}


# This function call the Shiny application process with inputs ui and server.
shinyApp(ui = ui, server = server)

