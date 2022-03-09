#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(fontawesome)
library(quantmod)



stocktab <-  menuItem("Stock Plot", tabName = "stock")
changetab <- menuItem("Change over Time", tabName = "change")
searchtab <-  menuItem("Search for your Stock", tabName = "search" )
earntab <- menuItem("Current Earnings", tabName = "earn")


sidebar <- dashboardSidebar(sidebarMenu(
  stocktab,
  changetab,
  searchtab,
  earntab))

body <- dashboardBody(tabItems(tabItem(tabName = "stock",h2("Select a stock"),
                                       dateInput("start_date", "Select Start Date", value = "2000-01-01"),
                                       dateInput("end_date", "Select End Date"),
                                       textInput("tick","Choose One Stock Ticker",value = "PEP"),
                                       submitButton(),
                                       plotlyOutput("stock_plot")),
                               tabItem(tabName = "change",h2("See how a stock changes over time")),
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
    STOCK <- getSymbols(input$tick, src = "yahoo", from = start, to = end, auto.assign = FALSE)
    autoplot(STOCK[,4])
    
    
    #chartSeries(STOCK)

  })
 
  
  #output$stock()
  
}

# Run the application 
shinyApp(ui = ui, server = server)

