## FIN 654 - Final Project

# Bankruptcy Prediction with Machine Learning

# Libraries:
library(caret)
library(C50)
## Adil Gokturk
setwd("~/Desktop/Spring2020/FIN654/Project")
getwd()
bank.data <- read.csv("Polish1Clean.csv")
head(bank.data, 1)
str(bank.data)
View(bank.data)

# 'data.frame':	3194 obs. of  66 variables!