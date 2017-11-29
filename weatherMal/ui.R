#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  
  # Application title
  # titlePanel("Weather Impact on Malaria Incidence"),
  fluidRow(
    column(4, selectInput("vars",
                          "Select Weather Variable:",
                          choices = c("Avg. Daily Temp" = 'tavg',
                                      "Daily rainfall in mm" = 'raintot',
                                      "Relative humidity in %" = 'rh',
                                      "Saturation vapor pressure deficit"='sd',
                                      "Surface barometric pressure"='psfc')))
  ),
  
  fluidRow(
    plotlyOutput("distPlot", width = "93%")
  ),
  
  
  hr()
  
))
