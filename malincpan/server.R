# Impact of Intervention on Malaria Incidence

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
library(ggiraph)
# load data
setwd("/srv/shiny-server/malaria_2k17")
final <- readRDS("./data/final.rds")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$plot1 <- renderPlotly({
    
    
    theGraph <- ggplot(final) +
      geom_smooth(aes(x = ITNprotn, y = inc1k, color = "ITN")) +
      geom_smooth(aes(x = IRSprotn, y = inc1k, color = "IRS")) +
      xlab("ITN or IRS Protection") +
      ylab("Incidence per 1000") + labs(color = "IRS or ITN")

    ggplotly(theGraph) 
    
  })
  


  
})


