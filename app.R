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
library(plotly)
library(ggpubr)

SYMBOL <- stockSymbols()


library(shiny)

stocktab <-  menuItem("Stock Plot", tabName = "stock")
changetab <- menuItem("Change over Time", tabName = "change")
searchtab <-  menuItem("Search for your Stock", tabName = "search" )
earntab <- menuItem("Current Earnings", tabName = "earn")


sidebar <- dashboardSidebar(sidebarMenu(
  stocktab,
  changetab,
  searchtab,
  earntab))

body <- dashboardBody(tabItems(tabItem(tabName = "stock",h2("Select a stock.
                                                            Will show the period you select or the time the stock was traded."),
                                       dateInput("start_date", "Select Start Date", value = "2000-01-01"),
                                       dateInput("end_date", "Select End Date"),
                                       selectInput("sname","Choose One Stock", choices = names(table(SYMBOL$Name)), selected = names(table(SYMBOL$Name))[789]),
                                       submitButton(),
                                       plotlyOutput("stock_plot")),
                               tabItem(tabName = "change",h2("See how much money you would have based on a past investment"),
                                       dateInput("start_invest", "Select Start Date", value = "2000-01-01"),
                                       dateInput("end_invest", "Select End Date"),
                                       selectInput("stockname","Choose One Stock", choices = names(table(SYMBOL$Name)), selected = names(table(SYMBOL$Name))[789]),
                                       selectInput("initial","Choose Amount", choices = 1:1000),
                                       submitButton(),
                                       verbatimTextOutput("money")),
                               tabItem(tabName = "search",h2("Search for your stock")),
                               tabItem(tabName = "earn",h2("Current Earnings of Stocks"))))

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Stocks App Project 2"),
   sidebar,
   body
))


server <- function(input, output) {
  
  output$stock_plot <- renderPlotly({
    start <- input$start_date
    end <- input$end_date
    TICK <- SYMBOL$Symbol[which(SYMBOL$Name == input$sname)]
    STOCK <- getSymbols(TICK, src = "yahoo", start = start, end = end, auto.assign = FALSE)
    autoplot(STOCK[,4]) + labs(title= paste(SYMBOL$Symbol[which(SYMBOL$Name == input$sname)], 
                                            "Stock Price"), y = "Price in USD", x = "Date")
    
    
    #chartSeries(STOCK)

  })

  output$money <- renderPrint({
    start_invest <- input$start_invest
    end_invest <- input$end_invest
    TICK <- SYMBOL$Symbol[which(SYMBOL$Name == input$stockname)]
    STOCK <- getSymbols(TICK, src = "yahoo", start = start_invest, end = end_invest, auto.assign = FALSE)
    start_price <- STOCK[1,4]
    start_price <- as.numeric(start_price)
    end_price <- tail(STOCK, 1)[,4]
    end_price <- as.numeric(end_price)
    change <- end_price - start_price
    percent_change <- change/start_price
    end_money <- as.numeric(input$initial) * (1+percent_change)
    end_money
  })
  
  #output$stock()
  
}


# Run the application 
shinyApp(ui = ui, server = server)

