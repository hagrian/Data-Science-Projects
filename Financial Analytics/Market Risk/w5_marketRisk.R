# Week 5 Market Risk

getwd()
## Download the data
data.all <- read.csv("eurostock.csv",
                     stringsAsFactors = FALSE)
# This will convert string dates to
# date objects below
str(data.all) # Check the structure and look for dates

str(row.names <- data.all$X) # We find that the first field X contains dates

date <- as.Date(row.names) # convert string dates to date objects
str(date) #Always look at structure to be sure
rownames(data.all) <- date
head(data.all)

## Subset the data using a start and
## end date
start.date <- "1975-06-02"
end.date <- "1990-12-30"
# First column looks for filtered
# dates, second and third columns
# pull out prices
price <- data.all[start.date <= date &
                    date <= end.date, c("RWE", "THYSSEN")]
# We add a check to ensure that price
# is a matrix and that ncol will work
if (!is.matrix(price)) price <- rbind(price,
                                      deparse.level = 0L)
str(price)

(end.idx <- dim(price)[1])

(price.2 <- rbind(price[1:5, ], price[(end.idx -
                                         4):end.idx, ]))

## Use scatter plots of the two price series along with their histograms to examine 
library(psych)
pairs.panels(price)
price.rownames <- rownames(price)
plot(as.Date(price.rownames), price[,"THYSSEN"], type="l",
     main="Thyssen stock price data", # title
     xlab="Date t", # x-axis label
     ylab=expression(Stock~price~price[t])) # y-axis label
plot(as.Date(price.rownames), price[,"RWE"], type="l",
     main="RWE stock price data", # title
     xlab="Date t", # x-axis label
     ylab=expression(Stock~price~price[t])) # y-axis label

# Prices are interesting but, as we have seen, are not stable predictors. Let’s
#transform them to returns next.

###########################################
# 4.4 Exercise: Now to the Matter at Hand


## Now to the matter at hand: value at risk and expected shortfall. These two
# measures are based on the quantiles of losses attributable to risk factors. Value at
# risk is the quantile at an ALPHA level of tolerance. Expected shortfall is the mean of the
# distribution beyond the value at risk threshold.
# To get losses attributable to market risk factors we compute log price differences
# (also called log price relatives). These can be interpreted as returns, or simply as
# percentage changes, in the risk factor prices. A plot lets us examine the results.

## Here we can compute two items
## together: log price differences,
## and their range (to bound a plot)
return.range <- range(return.series <- apply(log(price),
                                             2, diff)) # compute log-returns and range
return.range
plot(return.series, xlim = return.range,
     ylim = return.range, main = "Risk Factor Changes",
     cex = 0.2)

# Get last prices
price.last <- as.numeric(tail(price,
                              n = 1))
# Specify the positions
position.rf <- c(-30, 10)
# And compute the position weights
w <- position.rf * price.last
# Fan these across the length and
# breadth of the risk factor series
weights.rf <- matrix(w, nrow = nrow(return.series),
                     ncol = ncol(return.series), byrow = TRUE)
## We need to compute exp(x) - 1 for
## very small x: expm1 accomplishes
## this
loss.rf <- -rowSums(expm1(return.series) *
                      weights.rf)
summary(loss.rf)


loss.rf.df <- data.frame(Loss = loss.rf,
                         Distribution = rep("Historical",
                                            each = length(loss.rf)))
library(ggplot2)
ggplot(loss.rf.df, aes(x = Loss, fill = Distribution)) +
  geom_density(alpha = 0.2) + xlim(-100,
                                   100)

## Simple Value at Risk
alpha.tolerance <- 0.99
(VaR.hist <- quantile(loss.rf, probs = alpha.tolerance,
                      names = FALSE))
## Just as simple Expected shortfall
(ES.hist <- mean(loss.rf[loss.rf > VaR.hist]))
VaR.text <- paste("Value at Risk =",
                  round(VaR.hist, 2))
ES.text <- paste("Expected Shortfall =",
                 round(ES.hist, 2))
ggplot(loss.rf.df, aes(x = Loss, fill = Distribution)) +
  geom_density(alpha = 0.2) + geom_vline(aes(xintercept = VaR.hist),
                                         linetype = "dashed", size = 1, color = "blue") +
  geom_vline(aes(xintercept = ES.hist),
             size = 1, color = "blue") + xlim(0,
                                              200) + annotate("text", x = 40, y = 0.03,
                                                              label = VaR.text) + annotate("text",
                                                                                           x = 140, y = 0.03, label = ES.text)
# A box plot might also help us visualize the results without resorting to a probability
# distribution function.
ggplot(loss.rf.df, aes(x = Distribution,
                       y = Loss)) + geom_boxplot(outlier.size = 1.5,
                                                 outlier.shape = 21) + ylim(-250,
                                                                            10)
#####################################
# 4.5 Exercise: Carl Friedrich Gauss

# a quick comparison of the standard Gaussian and the Student’s t
# distributions.
# The functions rnorm and rt generate Gaussian and Student’s t variates,
# respectively.
# The functions qnorm and qt compute 
# the distance from the mean 
# (probability = 50%) for a given probability, here stored in alpha.tolerance.


library(mvtnorm) # Allows us to generate Gaussian and Student-t variates
library(ggplot2)
set.seed(1016)
n.sim <- 1000
z <- rnorm(n.sim)
t <- rt(n.sim, df = 5)
alpha.tolerance <- 0.95
(z.threshold <- qnorm(alpha.tolerance))
(t.threshold <- qt(alpha.tolerance, df = 5))


# Now make a data frame and plot with ggplot:
zt.df <- data.frame(Deviations = c(z,
                                   t), Distribution = rep(c("Gaussian",
                                                            "Student's t"), each = n.sim))
ggplot(zt.df, aes(x = Deviations, fill = Distribution)) +
  geom_density(alpha = 0.3) + geom_vline(aes(xintercept = z.threshold),
                                         color = "red", linetype = "dashed",
                                         size = 1) + geom_vline(aes(xintercept = t.threshold),
                                                                color = "blue", linetype = "dashed",
                                                                size = 1) + xlim(-3, 3)
# Let’s zoom in on the right tail with the xlim facet.
ggplot(zt.df, aes(x = Deviations, fill = Distribution)) +
  geom_density(alpha = 0.2) + geom_vline(aes(xintercept = z.threshold),
                                         color = "red", linetype = "dashed",
                                         size = 1) + geom_vline(aes(xintercept = t.threshold),
                                                                color = "blue", linetype = "dashed",
                                                                size = 1) + xlim(1, 5)

## Again computing returns as changes
## in the risk factors
return.series <- apply(log(price), 2,
                       diff) # compute risk-factor changes
price.last <- as.numeric(tail(price,
                              n = 1)) # reserve last price

# Specify the positions
position.rf <- c(-20, 10) # As before -30 short and 10 long 
# And compute the position weights
# directly again as before
(w <- position.rf * price.last)

mu.hat <- colMeans(return.series) # Mean vector mu; estimated = hat
Sigma.hat <- var(return.series) # Variance-covariance matrix Sigma
(loss.mean <- -sum(w * mu.hat)) # Mean loss


(loss.stdev <- sqrt(t(w) %*% Sigma.hat %*%
                      w)) # Standard deviation of loss

## Compute VaR and ES and return
alpha.tolerance <- 0.95
q.alpha <- qnorm(alpha.tolerance)
(VaR.varcov <- loss.mean + loss.stdev *
    q.alpha)

(ES.varcov <- loss.mean + loss.stdev *
    dnorm(q.alpha)/(1 - alpha.tolerance))


#plot 
VaR.text <- paste("Value at Risk =",
                  round(VaR.varcov, 2))
ES.text <- paste("Expected Shortfall =",
                 round(ES.varcov, 2))
ggplot(loss.rf.df, aes(x = Loss, fill = Distribution)) +
  geom_density(alpha = 0.2) + geom_vline(aes(xintercept = VaR.varcov),
                                         colour = "red", size = 1) + geom_vline(aes(xintercept = ES.varcov),
                                                                                colour = "blue", size = 1) + xlim(0,
                                                                                                                  200) + annotate("text", x = 30, y = 0.03,
                                                                                                                                  label = VaR.text) + annotate("text",
                                                                                                                                                               x = 120, y = 0.03, label = ES.text)

# Let's Go to Extremes
# Quantitative risk management 
library(QRM)
u <- quantile(loss.rf, alpha.tolerance,
              names = FALSE)
fit <- fit.GPD(loss.rf, threshold = u) # Fit GPD to the excesses
(xi.hat <- fit$par.ests[["xi"]]) # fitted xi, shape parameter tickness and peakedness in the tail distribution

(beta.hat <- fit$par.ests[["beta"]]) # fitted beta, location parameter


# Now for the closed form (no random variate simulation!) using the McNeil,
# Embrechts, and Frei (2015, chapter 5) formulae:
  # Pull out the losses over the
  # threshold and compute excess over
  # the threshold
loss.excess <- loss.rf[loss.rf > u] - u # compute the excesses over u
n.relative.excess <- length(loss.excess)/length(loss.rf) # = N_u/n
(VaR.gpd <- u + (beta.hat/xi.hat) * (((1 -
                                         alpha.tolerance)/n.relative.excess)^(-xi.hat) -
                                       1))
(ES.gpd <- (VaR.gpd + beta.hat - xi.hat *
              u)/(1 - xi.hat))

# How good a fit? This plot should look roughly uniform since the GPD excess loss
# function is a linear function of thresholds u.
gpd.density <- pGPD(loss.excess, xi = xi.hat,
                    beta = beta.hat)
gpd.density.df <- data.frame(Density = gpd.density,
                             Distribution = rep("GPD", each = length(gpd.density))) # This should be U[0,1]
ggplot(gpd.density.df, aes(x = Density,
                           fill = Distribution)) + geom_histogram()
# And it does look “uniform” enough (in a statistical sort of way as we perform
# eyeball econometrics again!).



# All together now
# Let’s graph the historical simulation, variance-covariance and GPD results together.
loss.plot <- ggplot(loss.rf.df, aes(x = Loss,
                                    fill = Distribution)) + geom_density(alpha = 0.2)
loss.plot <- loss.plot + geom_vline(aes(xintercept = VaR.varcov),
                                    colour = "red", linetype = "dashed",
                                    size = 1)
loss.plot <- loss.plot + geom_vline(aes(xintercept = ES.varcov),
                                    colour = "blue", linetype = "dashed",
                                    size = 1)
loss.plot <- loss.plot + geom_vline(aes(xintercept = VaR.gpd),
                                    colour = "red", size = 1)
loss.plot <- loss.plot + geom_vline(aes(xintercept = ES.gpd),
                                    colour = "blue", size = 1)
loss.plot <- loss.plot + xlim(0, 200)
loss.plot























