# Project Part 2
# BIT Group 1
# Application dashboard managing your portfolio

library(shiny)

# UI side of ProfitMaker
ui <- dashboardPage(skin = "yellow",
  
  dashboardHeader(title = "Profit Maker",
    dropdownMenuOutput("messageMenu")
  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
    ),
    fluidRow(
      column(width = 9,
        box(width = NULL, solidHeader = TRUE, status = "warning",
          # Show particular stock chart
          title = "Display a particular stock from your portfolio",
          br(),
          column(width = 12, plotOutput("stocks", width = "100%")),
          column(width = 6, br(), selectInput("listStock", label = "Select a symbol", "")),
          column(width = 6, br(), sliderInput("performStock", "Performance of the last x months", value = 3, min = 1, max = 48))
        ),
        tabBox(width = NULL, title = "Positions Information",
          tabPanel("Overview", dataTableOutput("listPortfolio")),
          tabPanel("Chart", plotOutput("repartitionChart")),
          tabPanel("Balance", fluidRow(
            valueBoxOutput("invested"),
            valueBoxOutput("return"),
            valueBoxOutput("left")
          ))
        )
      ),
      
      column(width = 3,
        box(width = NULL, status = "warning", title = "Parameters",
          # Select the amount of money invested
          numericInput("amountMoney", "Starting funds (€/$)", min = 0, value = 25000, step = 50),
          # Choose Market to use
          selectInput('marketPick', 'Stock Market', choices = c("Euronext" = "euronext", "Amex" = "amex", "Nasdaq" = "nasdaq", "Nyse" = "nyse"), selected = "euronext"),
          # Select the number of stocks in portfolio
          # The limit 100 is abritary picked as we need to pre generate the portfolio 
          sliderInput("numStock", "Portfolio size", value = 50, min = 1, max = 300),
          # Choose risks with Beta
          strong(paste("Risk (Beta) per Stocks")),
          column(width = 6, selectInput("methodBeta", "", choices = c("Max" = "max", "Min" = "min"), selected = "max")),
          column(width = 6, numericInput(inputId = "beta", "", value = 1, step = 0.1)),
          p(class = "text-muted",
            paste("Beta is a measure of the volatility of the stock in comparison to the market as a whole.")
          ),
          # Choose valuation index with RSI
          sliderInput(inputId = "riskRSI","Maximum RSI", value = 70, min = 0, max = 100, step = 1),
          p(class = "text-muted",
            paste("RSI provides a relative evaluation of the strength of a security's recent price performance. If a stock's RSI is above 70, the stock is consider as overbought.")
          ),
          # Pick stock by method chosen by the user
          selectInput("methodOfInvestment", "Method of Investement", choices = c("by RSI" = "rsi", "by Returns" = "returns"), selected = "returns"),
          # Base calculation on different returns date
          selectInput("rangeReturn", "Returns evaluation frequency", choices = c("Annually" = "a", "Daily" = "d", "Monthly" = "m"), selected = "m"),
          # Select transaction fees
          numericInput(inputId = "fees","Transactions fees (€/$)", value = 0, min = 0, max = 30, step = 0.1),
          p(class = "text-muted",
            paste("Changes make take a while to appear.")
          ),
          # Validate changes
          actionButton("submit", "Generate Portfolio")
        ),
          
        box(width = NULL, status = "warning", title = "Refresh intervals",
          # Disgusting but fast way to sec time - To fix someday
          selectInput("interval", "Update Current Stock Price", choices = c("5 minutes" = 300, "15 minutes" = 900, "30 minutes" = 1800, "3 hours" = 10800), selected = "1800"),
          selectInput("investmentUpdate", "Update Investment", choices = c("3 hours" = 10800, "12 hours" = 43200, "24 hours" = 86400, "3 days" = 259200, "7 days" = 604800, "14 days" = 1209600, "1 month" = 2592000), selected = "43200"),
          selectInput("methodOfSelection", "Method of selection", choices = c("by RSI" = "rsi", "by Performance" = "perf"), selected = "rsi"),
          actionButton("pause", "Pause"),
          actionButton("stop", "Stop"),
          p(class = "text-muted",
            paste("'Pause' stop investing but keep the current portfolio."),
            br(),
            paste("'Stop' sell all the positions.")
          ),
          downloadButton("download", "Download positions")
        )
      )
    )
  )
)