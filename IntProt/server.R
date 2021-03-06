
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
library(ggiraph)
library(gridExtra)

# load data
setwd("/srv/shiny-server/malaria_2k17")
final <- readRDS("./data/final.rds")
poly1 <- readShapePoly('./data/Moz_admin2.shp', IDvar="DISTCODE")


# Coverage of ITN
md_yr <- final %>%
  dplyr::select(DISTCODE, Epiyear, ITNprotn, IRSprotn) %>%
  group_by(DISTCODE, Epiyear) %>%
  summarize_all(funs(mean))

poly2 <- poly1
poly2 <- broom::tidy(poly2)
poly2$DISTCODE <- as.numeric(poly2$id)
poly2 <- left_join(poly2, md_yr)
poly2 <- left_join(poly2, final %>% 
                     dplyr::select(DISTCODE, District) %>%
                     distinct(DISTCODE, District))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  passData <- reactive({

    poly2 <- poly2 %>%
      filter(Epiyear == input$years)
      
    poly2
    
  })
  
  
  
  output$plot1 <- renderggiraph({
    
    graphData <- passData()
    
    p <- ggplot(data = graphData, # the input data
                aes(x = long, y = lat, fill = IRSprotn, group = group)) + # define variables
      geom_polygon() + # plot the districts
      geom_path(colour="black", lwd=0.05) + # district borders
      coord_equal() + # fixed x and y scales
      scale_fill_gradient2(low = "white", mid = "yellow", high = "green", # colors
                           midpoint = 0.5, name = "Protection") + # legend options
      theme(axis.text = element_blank(), # change the theme options
            axis.title = element_blank(), # remove axis titles
            axis.ticks = element_blank())
    

    my_gg <- p + geom_polygon_interactive(aes(data_id = District, tooltip = District), size = 3)  
    
    ggiraph(code = print(my_gg), width = 0.8)
    
  })
  
  output$plot2 <- renderggiraph({
    
    graphData <- passData()
    
    p <- ggplot(data = graphData, # the input data
                aes(x = long, y = lat, fill = ITNprotn, group = group)) + # define variables
      geom_polygon() + # plot the districts
      geom_path(colour="black", lwd=0.05) + # district borders
      coord_equal() + # fixed x and y scales
      scale_fill_gradient2(low = "white", mid = "yellow", high = "green", # colors
                           midpoint = 0.5, name = "Protection") + # legend options
      theme(axis.text = element_blank(), # change the theme options
            axis.title = element_blank(), # remove axis titles
            axis.ticks = element_blank())
    
    
    my_gg <- p + geom_polygon_interactive(aes(data_id = District, tooltip = District), size = 3)  
    
    ggiraph(code = print(my_gg), width = 0.8)
    
  })
  
})
