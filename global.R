# Project Part 2
# BIT Group 1
# Calculations for Stocks

# Disable warnings of Yahoo! Finance
options("getSymbols.yahoo.warning" = FALSE)

# Get All Stocks
getAllStocks <- function(market) {
  # Choose market
  if (identical(market, "nasdaq")) {
    symbols <- stockSymbols("NASDAQ")[,1]
  } else if (identical(market, "amex")) {
    symbols <- stockSymbols("AMEX")[,1]
  } else if (identical(market, "nyse")) {
    symbols <- stockSymbols("NYSE")[,1]
  } else if (identical(market, "euronext")) {
    # Get Euronext values
    source("preprocess/getYahooNameEU.R")
    symbols <- euronext$YSymbols
  } else {
    stop("Argument needed.", call. = TRUE)
  }
    
  stocks <- sapply(symbols, function(x) { try(na.approx(getSymbols(x, src = "yahoo", auto.assign = F, warnings = FALSE)), silent = TRUE ) })
      
  # Tidy data - remove value Yahoo does not have
  print(paste0(sum(lapply(stocks, class) == "try-error"), " values not found... removed"))
    
  # Clean list of stocks according to the remaining values
  stocks[which(lapply(stocks, class) == "try-error")] <- NULL
  if (identical(market, "euronext")) {
    euronext <- subset(euronext, euronext$YSymbols %in% names(stocks))
    symbols <- euronext$YSymbols
  } else {
    symbols <- subset(symbols, symbols %in% names(stocks))
  }
    
  # Keep stocks for quantmod but convert rest as dataframe
  stocks.df <- stocks
    
  for (i in 1:length(stocks.df)) {
    stocks.df[[i]] <- data.frame(Date = index(stocks[[i]]), coredata(stocks[[i]]))
    colnames(stocks.df[[i]]) <- c("Date","Open","High","Low","Close","Volume","Adjusted")
  }
  
  result <- list(stocks, stocks.df)
  return(result)
}

# Calculate for every stock the beta
calculateBeta <- function(input) {
  # Get the S&P500 index data
  sp500 <- getSymbols("^GSPC", auto.assign = FALSE)
  sp500 <- data.frame(Date = index(sp500), coredata(sp500))
  colnames(sp500) <- c("Date","Open","High","Low","Close","Volume","Adjusted")
  betalist <- c()
  i <- 1
  while (i < length(input)) {
    # Get all the dates that both have (otherwise we can't divide)
    joinDates <- merge(input[[i]], sp500, by = "Date")$Date
    index <- sp500[(sp500$Date %in% joinDates),]
    stock <- input[[i]][(input[[i]]$Date %in% joinDates),]
    stock2 <- stock[-1,]
    stock <- stock[-(nrow(stock)),]
    index2 <- index[-1,]
    index <- index[-(nrow(index)),]
    stock$Adjusted <- stock2$Adjusted / stock$Adjusted - 1
    index$Adjusted <- index2$Adjusted / index$Adjusted - 1
    # Calculate beta and Save the beta in a seperate list
    tryCatch({
      betalist[names(input)[i]] <- cov(stock$Adjusted, index$Adjusted) / var(index$Adjusted)
    }, error = function(e){
      stocks[[2]][i] <<- NULL
      stocks[[1]][i] <<- NULL
      input <- stocks[[2]]
      i <- i - 1
    })
    i <- i + 1
  }
  return(betalist)
}

# Calculate moving average of stocks
calculateMovingAverage <- function(input, long = 90, short = 20) {
  
  for (i in 1:length(input)) {
    vectorlong <- c()
    vectorshort <- c()
    for (j in 1:nrow(input[[i]])) {
      # Calculate the average of the last 90 and 30 days
      vectorlong <- c(vectorlong, mean(input[[i]][max(1,((-long + 1) + j)):j,"Adjusted"]))
      vectorshort <- c(vectorshort, mean(input[[i]][max(1,((-short + 1) + j)):j,"Adjusted"]))
    }
    # Store it in the dataframe
    input[[i]]["MALong"] <- vectorlong
    input[[i]]["MAShort"] <- vectorshort
  }
  return(input)
}

# Calculate the relative strength index of stocks
calculateRSI <- function(input, timeFrame = 14) {
  
  for (i in 1:length(input)) {
    vectorrsi <- c()
    for (j in 1:nrow(input[[i]])) {
      # Get the last n days and the last n - 1 days
      lastndays <- input[[i]][max(1,(-timeFrame + 1) + j):j,"Adjusted"]
      lastndaysminus1 <- input[[i]][max(1,(-timeFrame) + j):(max(1,(j - 1))),"Adjusted"]
      rsi <- 0
      # If there are 14 previous days
      if (length(lastndays) == length(lastndaysminus1)) {
        # Calculate the average up and down values
        yield <- lastndays / lastndaysminus1 - 1
        averageyieldminus <- mean(abs(yield[yield < 0]))
        averageyieldplus <- mean(yield[yield > 0])
        rsi <- 100 - (100 / (1 + averageyieldplus / averageyieldminus))
      }
      vectorrsi <- c(vectorrsi, rsi)
      
    }
    input[[i]]["RSI"] <- vectorrsi
  }
  return(input)
}

# Create Portfolio
# Warning the input of that function should be the return of getAllStocks
createPortfolio <- function(input, inputBeta, methodBeta, numMoney, numberOfStock, amountOfRisk, fees, rangeReturn = "m", rsi, methodOfInvestment, pBeta) {

  if (identical(rangeReturn, "d")) {
    cReturn <- dailyReturn
  } else if (identical(rangeReturn, "a")) {
    cReturn <- annualReturn
  } else {
    cReturn <- monthlyReturn
  }
  
  portfolio <- list()
  portfolioReturn <- c()
  portfolioBeta <- c()
  portfolioRSI <- c()
  
  for (i in 1:length(input[[2]])) {
    # Filter value based on Moving Average
    if (input[[2]][[i]]$Adjusted > last(input[[2]][[i]]$MALong) && input[[2]][[i]]$Adjusted > last(input[[2]][[i]]$MAShort)) {
      portfolio[i] <- input[[1]][i]
      # Get the last return - if not possible assign 0
      tryCatch({
        portfolioReturn[i] <- last(cReturn(portfolio[[i]]))
      }, error = function(e){
        portfolioReturn[i] <- 0
      })
      # Filter Beta & RSI
      portfolioBeta[i] <- inputBeta[i]
      portfolioRSI[i] <- last(input[[2]][[i]]$RSI)
    } else {
      portfolio[i] <- list(NULL)
    }
  }
  names(portfolio) <- names(input[[2]])
  
  # Filter remaining stocks on beta choosen and RSI
  if (identical(methodBeta, "min")) {
    decide <- portfolioBeta >= amountOfRisk & portfolioRSI <= rsi[2] & portfolioRSI >= rsi[1]
  } else {
    decide <- portfolioBeta <= amountOfRisk & portfolioRSI <= rsi[2] & portfolioRSI >= rsi[1]
  }
  portfolio <- portfolio[decide]
  portfolioReturn <- portfolioReturn[decide]
  portfolioBeta <- portfolioBeta[decide]
  portfolioRSI <- portfolioRSI[decide]
  
  # Clean value from portfolio
  portfolio <- portfolio[!sapply(portfolio, is.null)]
  portfolioReturn <- portfolioReturn[!sapply(portfolioReturn, is.na)]
  portfolioBeta <- portfolioBeta[!sapply(portfolioBeta, is.na)]
  portfolioRSI <- portfolioRSI[!sapply(portfolioRSI, is.na)]
  
  if (identical(methodOfInvestment, "returns")) {
    # Order by returns
    portfolio2 <- portfolio[order(portfolioReturn, decreasing = TRUE)]
  } else {
    # Order by lowest RSI if not returns
    portfolio2 <- portfolio[order(portfolioRSI, decreasing = FALSE)]
  }
  
  # Buy stocks
  moneySpend <- numberOfStock * fees
  # DISCLAIMER - No good way can be found to pick Portfolio automatically
  # Time two the fees to have enough money for selling
  moneyPerStock <- as.numeric(numMoney / numberOfStock - fees * 2)
  amountPerStock <- numeric()
  pricePerStock <- numeric()
  for (i in 1:min(numberOfStock, length(portfolio2))) {
    pricePerStock[i] <- as.numeric(getQuote(names(portfolio2[i]))$Last)
    amountPerStock[i] <- floor(moneyPerStock / pricePerStock[i])
    moneySpend <- moneySpend + pricePerStock[i] * amountPerStock[i]
  }
  while (moneySpend < numMoney && (numMoney - moneySpend) > min(pricePerStock, na.rm = TRUE)) {
    for (i in 1:min(numberOfStock, length(portfolio2), na.rm = TRUE)) {
      if (pricePerStock[i] < (numMoney - moneySpend)) {
        newStocks <- floor((numMoney - moneySpend) / pricePerStock[i])
        amountPerStock[i] <- amountPerStock[i] + newStocks
        moneySpend <- moneySpend + pricePerStock[i] * newStocks
      }
    }
  }
  pricePerStock <- as.numeric(pricePerStock)
  portfolio <- data.frame(Name = character(), Price = numeric(), Amount = numeric(), Current.Price = numeric(), Change = numeric(), stringsAsFactors = FALSE)
  for (i in 1:min(numberOfStock, length(portfolio2), na.rm = TRUE)) {
    portfolio[i,] <- c(names(portfolio2[i]), pricePerStock[i], amountPerStock[i], pricePerStock[i], 0)
  }
  portfolio$Price <- as.numeric(portfolio$Price)
  portfolio$Current.Price <- as.numeric(portfolio$Current.Price)
  portfolio$Amount <- as.numeric(portfolio$Amount)
  
  # Generate portfolio beta
  portfolioBeta <<- mean(portfolioBeta * portfolio$Amount/sum(portfolio$Amount))
  
  return(portfolio)
}

# Update Portfolio
updatePortfolio <- function(portfolio, market, balance, methodOfInvestment, settings) {
  
  # Refresh the whole stocks list
  stocks <- getAllStocks(market)

  # Re-Calculate moving average, rsi and beta for all stocks
  betalist <- calculateBeta(stocks[[2]])
  stocks[[2]] <- calculateMovingAverage(stocks[[2]])
  stocks[[2]] <- calculateRSI(stocks[[2]])
  
  if (identical(methodOfInvestment, "rsi")) {
    portfolioRSI <- c()
    # Get porfolio current RSI
    for (i in 1:nrow(portfolio)) {
      portfolioRSI[i] <- last(stocks[[2]][[portfolio$Name[i]]]$RSI)
    }
    worstStock <- portfolio[order(portfolioRSI, decreasing = TRUE),]
  } else if (identical(methodOfInvestment, "perf")) {
    worstStock <- portfolio[order(portfolio$Change, decreasing = FALSE),]
  } else {
    stop("Argument Needed", .call = TRUE)
  }
  
  reInvestNb <- abs(round(length(portfolio)/4, 0) - 1)
  worstStockName <- worstStock$Name[1:reInvestNb]
  
  balance["Money Lost"] <- sum(worstStock$Current.Price[1:reInvestNb] - (worstStock$Price[1:reInvestNb] *  as.numeric(worstStock$Change[1:reInvestNb])) / 100)
  balance["Money Left"] <- sum(worstStock$Current.Price[1:reInvestNb]) + balance["Money Left"] - reInvestNb * as.numeric(settings["fees"])

  # Remove "sold" stocks from portfolio
  portfolio[worstStockName] <- NULL
  
  # Do not reinvest into stocks sold
  stocks[[1]][worstStockName] <- list(NULL)
  stocks[[2]][worstStockName] <- list(NULL)
  
  # Create new portfolio using money remaining
  newPortfolio <- createPortfolio(input = stocks, inputBeta = as.numeric(betalist), methodBeta = settings["methodBeta"], numMoney = as.numeric(balance["Money Left"]), numberOfStock = as.numeric(reInvestNb), amountOfRisk = as.numeric(settings["beta"]), fees = as.numeric(settings["fees"]), rangeReturn = settings["RangeReturn"], rsi = c(0, as.numeric(settings["RSI"])), methodOfInvestment = settings["methodOfInvestment"], pBeta = settings["pBeta"])
  
  # Update balance
  balance["Money Invested"] <- balance["Money Invested"] + crossprod(as.numeric(newPortfolio$Price), as.numeric(newPortfolio$Amount)) + reInvest * as.numeric(settings["fees"])
  balance["Money Left"] <- balance["Money Left"] - crossprod(as.numeric(newPortfolio$Price), as.numeric(newPortfolio$Amount))
  
  # Append new stocks to portfolio
  portfolio <- rbind(portfolio, newPortfolio)
  
  # Return new balance and portfolio
  combined <- list(portfolio, balance)
  return(combined)
}

# Get current prices from portfolio
updateCurrentPortfolioPrices <- function(portfolio) {
  currentPrices <- getQuote(portfolio$Name)[2]
  portfolio$Current.Price <- currentPrices$Last
  portfolio$Change <- round(((portfolio$Current.Price / portfolio$Price) - 1) * 100, 2)
  return(portfolio)
}

# List the stocks name of your portfolio
listStockNames <- function(portfolio) {
  return(as.character(portfolio$Name))
}