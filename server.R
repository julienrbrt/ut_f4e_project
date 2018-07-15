# Project Part 2
# BIT Group 1
# Application dashboard managing your portfolio

library(shiny)

source("dependencies.R")
source("global.R")
# source("preRun.R")

# Server side of ProfitMaker
server <- function(input, output, session) {
  
  # Store last value so we can compare it
  lastMarket <- "euronext"
  
  # Make chart usable with default temporary portfolio
  updateSelectInput(session, "listStock", choices = listStockNames(portfolio))
  
  # Create a balance variable to be updated
  balance <- vector(mode = "numeric", length = 3)
  names(balance) <- c("Money Invested", "Money Left", "Money Lost")
  
  # Create a setting variable for updating stocks
  settings <- vector(mode = "character", length = 6)
  names(settings) <- c("beta", "methodBeta", "RangeReturn", "fees", "RSI", "methodOfInvestment")
  
  # Create reactive value for update
  rv <- reactiveValues(portfolio = portfolio, stocks = stocks, betalist = betalist, balance = balance, settings = settings, lastMarket = lastMarket)
  
  observeEvent(input$submit, {

    withProgress(message = "Fetching data", value = 0, {
      
      if (!identical(rv$lastMarket, input$marketPick)) {
        # Get stock from the Internet
        rv$lastMarket <- input$marketPick
        print(paste0("Switching to: ", rv$lastMarket))
        rv$stocks <- getAllStocks(rv$lastMarket)
        # Calculate moving average, rsi and beta for all stocks
        incProgress(2/10, detail = "Calculating Beta")
        rv$betalist <- calculateBeta(rv$stocks[[2]])
        incProgress(2/10, detail = "Calculating MVA")
        rv$stocks[[2]] <- calculateMovingAverage(rv$stocks[[2]])
        incProgress(2/10, detail = "Calculating RSI")
        rv$stocks[[2]] <- calculateRSI(rv$stocks[[2]])
      }
      incProgress(2/10, detail = "Preparing your portfolio")
      rv$portfolio <- createPortfolio(input = rv$stocks, inputBeta = rv$betalist, methodBeta = input$methodBeta, numMoney = input$amountMoney, numberOfStock = input$numStock, amountOfRisk = input$beta, fees = input$fees, rangeReturn = input$rangeReturn, rsi = c(0, input$riskRSI), methodOfInvestment = input$methodOfInvestment)
      # Save settings
      rv$settings <- c(input$beta, input$methodBeta, input$rangeReturn, input$fees, input$riskRSI, input$methodOfInvestment)
      # Send message about money invested
      output$messageMenu <- renderMenu({
        dropdownMenu(type = "messages", 
          notificationItem(
            text = paste0(crossprod(as.numeric(rv$portfolio$Price), as.numeric(rv$portfolio$Amount)) + input$numStock * input$fees, " €/$ have been invested."),
            icon = icon("life-ring"),
            status = "success"
          )
        )
      })
      # Update stocks name for chart
      updateSelectInput(session, "listStock", choices = listStockNames(rv$portfolio))
    })
  })
  
  # Plot chart of selected stocks
  output$stocks <- renderPlot({
    # Plot only the last 3 months chart
    candleChart(rv$stocks[[1]][[input$listStock]], name = paste0("Historical values of ", input$listStock), theme = "white", subset = paste0("last ", input$performStock, " months"), type = 'candles')
    # Add Moving average
    addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "brown")
    # Add Relative Strengh Index
    # addRSI(n = 14, wilder = TRUE)
  })

  # Show current portfolio
  output$listPortfolio <- renderDataTable(data.frame(Symbols = listStockNames(rv$portfolio), Amount = rv$portfolio$Amount, "Buying Price" = rv$portfolio$Price, "Current Price" = rv$portfolio$Current.Price, "Change (%)" = rv$portfolio$Change),
    options = list(pageLength = 10, dom = "tp")
  )

  output$repartitionChart <- renderPlot(
    barplot(as.numeric(rv$portfolio$Amount), names = rv$portfolio$Name, xlab = "Company names", ylab = "Amount of stocks", col = "lightblue")
  )
  
  # Show balance info
  output$invested <- renderValueBox({
    valueBox(
      value = round(rv$balance["Money Invested"], 2),
      subtitle = "Invested",
      icon = icon("money"),
      color = "aqua"
    )
  })
  
  output$return <- renderValueBox({
    valueBox(
      if (rv$balance["Money Invested"] > 0) {
        round((sum(rv$balance)/rv$balance["Money Invested"] - 1)*100, 4) }
      else {
        0
      },
      "ROI (%)",
      icon = icon("line-chart"),
      color = "yellow"
    )
  })
  
  output$left <- renderValueBox({
    valueBox(
      round(rv$balance["Money Left"], 2),
      "Not Invested",
      icon = icon("money"),
      color = "red"
    )
  })
  
  # Update the portfolio and the investment according to the value set by the user
  # Not triggered at first launch with 'ignoreInit = TRUE'
  observeEvent(input$investmentUpdate, {
    
    invalidateLater(as.numeric(input$investmentUpdate) * 1000, session)
    
    withProgress(message = "Updating Investment", value = 1/2, {
      combined <- updatePortfolio(rv$portfolio, rv$lastMarket, rv$balance, input$methodOfSelection, rv$settings)
      rv$portfolio <- combined[1]
      rv$balance <- combined[2]
      incProgress(1/2, detail = "Done")
    })
  }, ignoreInit = TRUE)
  
  # Update the current stocks price with the value set by the user
 observe({

    invalidateLater(as.numeric(input$interval) * 1000, session)
    
    # Launch that only once
    once <- 0
    if (identical(once, 0)) {
      rv$balance["Money Invested"] <- crossprod(as.numeric(portfolio$Price), as.numeric(portfolio$Amount)) + input$fees * input$numStock
      rv$balance["Money Left"] <- input$amountMoney - crossprod(as.numeric(rv$portfolio$Price), as.numeric(rv$portfolio$Amount)) - input$fees * input$numStock
      rv$balance["Money Lost"] <- 0
      once <- 1
    }
    
    withProgress(message = "Updating stocks", value = 1/2, {
      rv$portfolio <- updateCurrentPortfolioPrices(rv$portfolio)
      incProgress(1/2, detail = "Done")
    })
  })
  
  # Download positions of portolio
  output$download <- downloadHandler(
    filename = function() {
      paste("positions-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$portfolio, file, row.names = FALSE)
    }
  )
  
  # Pause the investment - i.e Stop to buy / sell stocks - Not Implemented
  observeEvent(input$pause, {
    
    # Send message about the stock sold and the new balance
    output$messageMenu <- renderMenu({
      dropdownMenu(type = "messages", 
        notificationItem(
          text = "Profit Maker will now stop trading for you.",
          icon = icon("info"),
          status = "success"
       )
      )
    })
    
  })
  
  # Stop the investment - i.e Sell every stocks - Not Implemented
  observeEvent(input$stop, {
    
    # rv$balance["Money Left"] <- rv$balance["Money Left"] + rv$balance["Money Invested"]
    # rv$balance["Money Invested"] <- 0

    # Send message about the stock sold and the new balance
    output$messageMenu <- renderMenu({
      dropdownMenu(type = "messages", 
        notificationItem(
          text = paste0("All your positions have been sold, you have now", rv$balance["Money Left"], "€/$"),
          icon = icon("warning"),
          status = "success"
        )
      )
    })
    
  })
}