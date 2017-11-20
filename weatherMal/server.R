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
library(plotly)

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
  

  
  # 
  output$distPlot <- renderPlotly({
    
  
    
    theGraph <- ggplot(final) +
      geom_smooth(aes(x = get(input$vars), y = inc1k, colour="No Lag")) +
      geom_smooth(aes(x = get(paste0(input$vars,"_lag_2")), y = inc1k, colour="Lag 2wk")) +
      geom_smooth(aes(x = get(paste0(input$vars,"_lag_4")), y = inc1k, colour="Lag 4wk")) +
      geom_smooth(aes(x = get(paste0(input$vars,"_lag_8")), y = inc1k, colour="Lag 8wk")) +
      # scale_colour_manual(name=lab(), values=c("blue", "red", "green", "yellow")) +
      xlab(lab()) +
      ylab("Incidence per 1000")
    
    
    print(ggplotly(theGraph)  %>%
            layout( legend=list(orientation = 'h',
                                xanchor = "center",
                                yanchor = "bottom",
                                x = 0.5) ))
    
  })
  
})


