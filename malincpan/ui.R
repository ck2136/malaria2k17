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
  # titlePanel("ITN and IRS Protection on Malaria Incidence"),
  # fluidRow(
  #   column(5, selectInput("cats",
  #                         "Select Stratifying Variable:",
  #                         choices = c("Region" = "Region",
  #                                     "Province" = "Province")), align = 'center')
  #   column(5, selectInput("cats",
  #                         "Select Stratifying Variable:",
  #                         choices = c("Region" = "Region",
  #                                     "Province" = "Province")), align = 'center')
  # ),
  # 
  
  div(plotlyOutput("plot1"), align = 'center')

  )
  
)
