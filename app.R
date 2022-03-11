#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(fontawesome)
library(quantmod)
library(dplyr)
library(fpp3)
library(ggplot2)
library(ggeasy)
library(plotly)
library(ggpubr)
library(tidyquant)
library(TTR)
library(scales)

SYMBOL <- stockSymbols()


library(shiny)
library(shinyWidgets)

stocktab <- menuItem("Stock Plot", tabName = "stock")
changetab <- menuItem("Change over Time", tabName = "change")
searchtab <- menuItem("Plot Multiple Stocks", tabName = "MultiplePlot")
earntab <- menuItem("Current Earnings", tabName = "earn")


sidebar <- dashboardSidebar(sidebarMenu(
  stocktab,
  changetab,
  searchtab,
  earntab
))

body <- dashboardBody(tabItems(
  tabItem(
    tabName = "stock", h2("Select a stock.
                                                            Will show the period you select or the time the stock was traded."),
    dateInput("start_date", "Select Start Date", value = "2000-01-01"),
    dateInput("end_date", "Select End Date"),
    selectInput("sname", "Choose One Stock", choices = names(table(SYMBOL$Name)), selected = names(table(SYMBOL$Name))[789]),
    submitButton(),
    plotlyOutput("stock_plot")
  ),
  tabItem(
    tabName = "change", h2("See how much money you would have based on a past investment"),
    dateInput("start_invest", "Select Start Date", value = "2000-01-01"),
    dateInput("end_invest", "Select End Date"),
    selectInput("stockname", "Choose One Stock", choices = names(table(SYMBOL$Name)), selected = names(table(SYMBOL$Name))[789]),
    selectInput("initial", "Choose Amount", choices = 1:1000),
    submitButton(),
    textOutput("money")
  ),
  tabItem(
    tabName = "MultiplePlot", h2("Select a date range and multiple stocks to see them graphed."),
    dateInput("start_invest_multi2", "Select Start Date", value = "2000-01-01"),
    dateInput("end_invest_multi2", "Select End Date"),
    selectInput("stocknamecomp1", "Choose Stock 1", choices = names(table(SYMBOL$Name)), selected = names(table(SYMBOL$Name))[789]),
    plotlyOutput("stonk"),
    selectInput("stocknamecomp2", "Choose Stock 2", choices = names(table(SYMBOL$Name)), selected = names(table(SYMBOL$Name))[789]),
    plotlyOutput("stonk2"),
    submitButton()
  ),
  tabItem(
    tabName = "earn", h2("Current Earnings of Stocks"),
    dateInput("start_invest_multi", "Select Start Date", value = "2000-01-01"),
    dateInput("end_invest_multi", "Select End Date"),
    selectInput("stockname1multi", "Choose Stock 1", choices = names(table(SYMBOL$Name)), selected = names(table(SYMBOL$Name))[789]),
    selectInput("stockname2multi", "Choose Stock 2", choices = names(table(SYMBOL$Name)), selected = names(table(SYMBOL$Name))[789]),
    selectInput("initial1multi", "Choose Amount of Stock 1", choices = 1:1000),
    selectInput("initial2multi", "Choose Amount of Stock 2", choices = 1:1000),
    submitButton(),
    textOutput("money2")
  )
))

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Stocks App Project 2"),
    sidebar,
    body
  )
)


server <- function(input, output) {
  output$stock_plot <- renderPlotly({
    start <- input$start_date
    end <- input$end_date
    TICK <- SYMBOL$Symbol[which(SYMBOL$Name == input$sname)]
    STOCK <- getSymbols(TICK, src = "yahoo", from = start, to = end, auto.assign = FALSE)
    autoplot(STOCK[, 4]) + labs(title = paste(
      SYMBOL$Symbol[which(SYMBOL$Name == input$sname)],
      "Stock Price"
    ), y = "Price in USD", x = "Date")
  })

  output$money <- renderText({
    start_invest <- input$start_invest
    end_invest <- input$end_invest
    TICK <- SYMBOL$Symbol[which(SYMBOL$Name == input$stockname)]
    STOCK <- getSymbols(TICK, src = "yahoo", from = start_invest, to = end_invest, auto.assign = FALSE)
    start_price <- STOCK[1, 4]
    start_price <- as.numeric(start_price)
    end_price <- tail(STOCK, 1)[, 4]
    end_price <- as.numeric(end_price)
    change <- end_price - start_price
    percent_change <- change / start_price
    end_money <- as.numeric(input$initial) * (1 + percent_change)
    paste("Potential Money Made is ", dollar(end_money))
  })

  output$money2 <- renderText({
    start_invest <- input$start_invest_multi
    end_invest <- input$end_invest_multi
    TICK <- SYMBOL$Symbol[which(SYMBOL$Name == input$stockname1multi)]
    TICK2 <- SYMBOL$Symbol[which(SYMBOL$Name == input$stockname2multi)]
    STOCK <- getSymbols(TICK, src = "yahoo", from = start_invest, to = end_invest, auto.assign = FALSE)
    STOCK2 <- getSymbols(TICK2, src = "yahoo", from = start_invest, to = end_invest, auto.assign = FALSE)
    start_price <- STOCK[1, 4]
    start_price2 <- STOCK2[1, 4]
    start_price <- as.numeric(start_price)
    start_price2 <- as.numeric(start_price2)
    end_price <- tail(STOCK, 1)[, 4]
    end_price2 <- tail(STOCK2, 1)[, 4]
    end_price <- as.numeric(end_price)
    end_price2 <- as.numeric(end_price2)
    change <- end_price - start_price
    change2 <- end_price2 - start_price2
    percent_change <- change / start_price
    percent_change2 <- change2 / start_price2
    end_money <- as.numeric(input$initial1multi) * (1 + percent_change)
    end_money2 <- as.numeric(input$initial2multi) * (1 + percent_change2)
    total <- sum(end_money + end_money2)
    paste("Current Earnings of Stock is ", dollar(total))
  })
  output$stonk <- renderPlotly({
    start_invest <- input$start_invest_multi2
    end_invest <- input$end_invest_multi2
    TICKS1 <- SYMBOL$Symbol[which(SYMBOL$Name == input$stocknamecomp1)]
    STOCKS1 <- getSymbols(TICKS1, src = "yahoo", from = start_invest, to = end_invest, auto.assign = FALSE)
    autoplot(STOCKS1[, 4]) + labs(title = paste(
      SYMBOL$Symbol[which(SYMBOL$Name == input$stocknamecomp1)],
      "Stock Price"
    ), y = "Price in USD", x = "Date")
  })
  output$stonk2 <- renderPlotly({
    start_invest <- input$start_invest_multi2
    end_invest <- input$end_invest_multi2
    TICKS2 <- SYMBOL$Symbol[which(SYMBOL$Name == input$stocknamecomp2)]
    STOCKS2 <- getSymbols(TICKS2, src = "yahoo", from = start_invest, to = end_invest, auto.assign = FALSE)
    autoplot(STOCKS2[, 4]) + labs(title = paste(
      SYMBOL$Symbol[which(SYMBOL$Name == input$stocknamecomp2)],
      "Stock Price"
    ), y = "Price in USD", x = "Date")
  })
}


# Run the application
shinyApp(ui = ui, server = server)
