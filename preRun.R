# Project Part 2
# BIT Group 1
# Load easily all the dependencies and prefetch the packages

source("dependencies.R")
source("global.R")

# Pre-generate a portfolio with default values
stocks <- getAllStocks("euronext")
betalist <- calculateBeta(stocks[[2]])
stocks[[2]] <- calculateMovingAverage(stocks[[2]])
stocks[[2]] <- calculateRSI(stocks[[2]])

portfolio <- createPortfolio(input = stocks, inputBeta = betalist, methodBeta = "max", numMoney = 25000, numberOfStock = 50, amountOfRisk = 0.8, fees = 0, rangeReturn = "m", rsi = c(0, 70), methodOfInvestment = "returns", pBeta = 0)