#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(lattice) # for graphics 
library(RColorBrewer)
library(sp)
library(maptools)
library(latticeExtra)
library(rgdal)
library(tmap)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(EpiWeek)

# load data
final <- readRDS("/home/ck1/Documents/PythonR/Project/data/final.rds")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  lab <- reactive({
    y=c()
    if(input$vars == "tavg") {
      y="Daily Average Temp (C)"
    }
    if(input$vars == "raintot") {
      y="Daily Rainfall (mm)"
    }
    if(input$vars == "rh") {
      y="Relative Humidity (%)"
    }
    if(input$vars == "sd") {
      y="Saturation Vapor\n Pressure Deficit (mmHg)" 
    }
    if(input$vars == "psfc") {
      y="Surface Barometric\n Pressure (mmHg)"
    }
    y
    })
  
  # lab2 <- reactive({
  #   y=c()
  #   if(input$cats == "Region") {
  #     y="Region"
  #   }
  #   if(input$cats == "Province") {
  #     y="Province"
  #   }
  #   y
  # })
  # 
  # 
  output$distPlot <- renderPlot({
    
    
    theGraph <- ggplot(final, aes(x = epiweekToDate(final$Epiyear, final$Epiweek)[[1]], y = final[input$vars], colour = get(input$cats), group = get(input$cats))) +
      geom_smooth() +
      xlab("Years") +
      ylab(lab()) + labs(colour=input$cats)
    
    print(theGraph)
    
  })
  
})


