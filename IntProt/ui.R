#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggiraph)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  
  # Application title
  # titlePanel("ITN and IRS Protection"),
  fluidRow(
    column(5, sliderInput("years",
                "Choose a year:",
                min = 2010,
                max = 2017,
                value = 2010))
  ),
  
  fluidRow(
    # plotOutput("distPlot")
    
    splitLayout(cellWidths = 430, ggiraphOutput("plot1", width = "100%"), ggiraphOutput("plot2", width = "100%"))
  )
  
))
