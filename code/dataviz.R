##############################################
# Data Visualization and Exploratory Analyasis
# Created by: CK
# Created on: 10/26/2017
# Modified by: CK
# Modified on: 10/26/2017
##############################################

# WARNING: Run the data_management.R file to get the necessary files for visualization and exploratory analysis here 

#### 1. Loading Libraries and Data ####
library(lattice) # for graphics 
library(RColorBrewer)
library(sp)
library(maptools)
library(latticeExtra)
library(rgdal)
library(tmap)
library(animation)
library(cowplot)
library(ggpubr)
library(gganimate)
library(dplyr)

final <- readRDS("~/Documents/PythonR/Project/data/final.rds")
poly1 <- readShapePoly('/home/ck1/Documents/PythonR/Project/data/Moz_admin2.shp', IDvar="DISTCODE")


#### 2. Visualization ####
#2.1 Aggregate (2010-2017) Cases and Average rainfall,temp and etc within Mozambique
poly1@data <- left_join(poly1@data, aggregate(cases ~ DISTCODE, FUN = sum, data = final))
poly1@data <- left_join(poly1@data, aggregate(inc1k ~ DISTCODE, FUN = mean, data = final))
qtm(poly1,'cases') # Sum of all cases over 2010-2017 overall cases...
qtm(poly1,'inc1k') # average cases over 2010 to 2017 

poly1@data <- left_join(poly1@data, final %>%
                          select('tavg','raintot','rh','sd','psfc') %>%
                          summarize_all(funs(mean)))
tm_shape(poly1) +
  tm_fill("tavg", fill.title = "Avg Temp", scale = 0.8, alpha = 0.5, title = "Avg Temp")
tm_shape(poly1) +
  tm_fill("raintot", fill.title = "Avg Total Rain", scale = 0.8, alpha = 0.5, title = "Avg Daily Tot Rain")
tm_shape(poly1) +
  tm_fill("rh", fill.title = "Avg Rel Humidity (%)", scale = 0.8, alpha = 0.5, title = "Avg Rel. Humidity (%)")
tm_shape(poly1) +
  tm_fill("sd", fill.title = "Avg Sat. Vapro Press (mmHg)", scale = 0.8, alpha = 0.5, title = "Avg Saturation Vapor Pressure")

#2.2 Over time changes in cases and etc
# Need to aggregate information by year... wks too long :) 1.8 gbs
md_yr <- final %>%
  select(DISTCODE, Epiyear, inc1k) %>%
  group_by(DISTCODE, Epiyear) %>%
  summarize_all(funs(mean)) 

poly2 <- poly1
poly2 <- broom::tidy(poly2)
poly2$DISTCODE <- as.numeric(poly2$id)
poly2 <- left_join(poly2, md_yr)

# 2.3 Multi year cases/1000 faceted ggplot probably just do the cases... not the other plots too long

# 2.3.1 Just Graph
p = ggplot(data = poly2, # the input data
           aes(x = long, y = lat, fill = inc1k, group = group)) + # define variables
  geom_polygon() + # plot the districts
  geom_path(colour="black", lwd=0.05) + # district borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ Epiyear) + # one plot per time slice
  scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", # colors
                       midpoint = 30, name = "Inc per 1000") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks



# 2.3.2 Make Animation
p = ggplot(data = poly2, # the input data
       aes(x = long, y = lat, fill = inc1k, group = group, frame = Epiyear)) + # define variables
  geom_polygon() + # plot the districts
  geom_path(colour="black", lwd=0.05) + # district borders
  coord_equal() + # fixed x and y scales
  #facet_wrap(~ Epiyear) + # one plot per time slice
  scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", # colors
                       midpoint = 30, name = "Inc per 1000") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks

# needs gganimate package
gganimate(p, "mb_inc1k.gif", interval=0.2)


#2.3.2 Make multiyear panel plot using spplot
library(zoo)
library(xts)
library(tidyr)

md_yr <- final %>%
  select(DISTCODE, Epiyear, inc1k) %>%
  group_by(DISTCODE, Epiyear) %>%
  summarize_all(funs(mean)) 


temp <- spread(md_yr, Epiyear, inc1k)
poly2 <- poly1
poly2@data <- left_join(poly2@data, temp)
rownames(poly2@data) <- poly2@data$DISTCODE
poly2@data$SP_ID <- NULL
poly2@data$DISTCODE <- NULL
spcol <- function(num) {
  x = c()
  for(i in 1:num) {
    x[i] = paste0("X",round(((i-1)*365.2422) + 1, 0) )
  }
  x
}
x <- spcol(8)
colnames(poly2@data) <- x

spplot(poly2, names.attr = unique(md_yr$Epiyear), as.table = TRUE, colorkey = TRUE)


# 2.4 Average incidence, tavg, raintot, rh, sd, and psfc over time graph
#incidence

# Avg Incidence per 1000 over time by Region
ggplot(final, aes(x = epiweekToDate(final$Epiyear, final$Epiweek)[[1]], y = inc1k, colour = Region, group = Region)) +
  geom_smooth() +
  xlab("Years") +
  ylab("Incidence per 1000")


#tavg


ggarrange(ggplot(final, aes(x = epiweekToDate(final$Epiyear, final$Epiweek)[[1]], y = tavg, colour = Region, group = Region)) +
            geom_smooth() +
            xlab("Years") +
            ylab("Daily Average Temp (C)"),
          ggplot(final, aes(x = epiweekToDate(final$Epiyear, final$Epiweek)[[1]], y = raintot, colour = Region, group = Region)) +
            geom_smooth()+
            xlab("Years") +
            ylab("Daily Rainfall (mm)"),
          ggplot(final, aes(x = epiweekToDate(final$Epiyear, final$Epiweek)[[1]], y = rh, colour = Region, group = Region)) +
            geom_smooth()+
            xlab("Years") +
            ylab("Relative Humidity (%)"),
          ggplot(final, aes(x = epiweekToDate(final$Epiyear, final$Epiweek)[[1]], y = sd, colour = Region, group = Region)) +
            geom_smooth()+
            xlab("Years") +
            ylab("Saturation Vapor\n Pressure Deficit (mmHg)"),
          ggplot(final, aes(x = epiweekToDate(final$Epiyear, final$Epiweek)[[1]], y = psfc, colour = Region, group = Region)) +
            geom_smooth()+
            xlab("Years") +
            ylab("Surface Barometric\n Pressure (mmHg)"),
          labels = c("A", "B","C","D","E"),
          common.legend = TRUE, legend = "bottom"
)

ggplot(final, aes(x = epiweekToDate(final$Epiyear, final$Epiweek)[[1]], y = final['tavg'], colour = Region, group = Region)) +
  geom_smooth() +
  xlab("Years") +
  ylab("Daily Average Temp (C)")


#### 3. Exploratory Analyasis with Independent Variables ####

ggarrange(ggplot(final, aes(x = raintot, y = inc1k, colour = Region, group = Region)) +
            geom_smooth() +
            xlab("Avg Total Rain a week") +
            ylab("Incidence per 1000") ,
          ggplot(final, aes(x = raintot_lag_2, y = inc1k, colour = Region, group = Region)) +
            geom_smooth() +
            xlab("2 Wks Lagged Avg Total Rain a Week") +
            ylab("Incidence per 1000") ,
          ggplot(final, aes(x = raintot_lag_4, y = inc1k, colour = Region, group = Region)) +
            geom_smooth() +
            xlab("4 Wks Lagged Avg Total Rain a week") +
            ylab("Incidence per 1000"),
          ggplot(final, aes(x = raintot_lag_8, y = inc1k, colour = Region, group = Region)) +
            geom_smooth() +
            xlab("8 Wks Lagged Avg Total Rain a week") +
            ylab("Incidence per 1000"),
          labels = c("A","B","C","D") ,
          common.legend = TRUE, legend = "bottom"
)

# tavg
# some sort of interaction
ggarrange(
  ggplot(final, aes(x = tavg, y = inc1k, colour = Region, group = Region)) +
    geom_smooth(),
  ggplot(final, aes(x = tavg_lag_2, y = inc1k, colour = Region, group = Region)) +
    geom_smooth(),
  ggplot(final, aes(x = tavg_lag_4, y = inc1k, colour = Region, group = Region)) +
    geom_smooth(),
  ggplot(final, aes(x = tavg_lag_8, y = inc1k, colour = Region, group = Region)) +
    geom_smooth(),
  labels = c("A","B","C","D") ,
  common.legend = TRUE, legend = "bottom"
)

#rh
ggarrange(
  ggplot(final, aes(x = rh, y = inc1k, colour = Region, group = Region)) +
    geom_smooth(),
  ggplot(final, aes(x = rh_lag_2, y = inc1k, colour = Region, group = Region)) +
    geom_smooth(),
  ggplot(final, aes(x = rh_lag_4, y = inc1k, colour = Region, group = Region)) +
    geom_smooth(),
  ggplot(final, aes(x = rh_lag_8, y = inc1k, colour = Region, group = Region)) +
    geom_smooth(),
  labels = c("A","B","C","D") ,
  common.legend = TRUE, legend = "bottom"
)

#sd
ggarrange(
  ggplot(final, aes(x = sd, y = inc1k, colour = Region, group = Region)) +
    geom_smooth(),
  ggplot(final, aes(x = sd_lag_8, y = inclag2, colour = Region, group = Region)) +
    geom_smooth(),
  ggplot(final, aes(x = sd_lag_4, y = inclag4, colour = Region, group = Region)) +
    geom_smooth(),
  ggplot(final, aes(x = sd_lag_8, y = inclag8, colour = Region, group = Region)) +
    geom_smooth(),
  labels = c("A","B","C","D") ,
  common.legend = TRUE, legend = "bottom"
)

#psfc
ggarrange(
  ggplot(final, aes(x = psfc, y = inc1k, colour = Region, group = Region)) +
    geom_smooth(),
  ggplot(final, aes(x = psfc_lag_2, y = inc1k, colour = Region, group = Region)) +
    geom_smooth(),
  ggplot(final, aes(x = psfc_lag_4, y = inc1k, colour = Region, group = Region)) +
    geom_smooth(),
  ggplot(final, aes(x = psfc_lag_8, y = inc1k, colour = Region, group = Region)) +
    geom_smooth(),
  labels = c("A","B","C","D") ,
  common.legend = TRUE, legend = "bottom"
)

#protection variable
ggarrange(
  ggplot(final, aes(x = ITNprotn, y = inc1k, colour = Region, group = Region)) +
    geom_smooth() +
    xlab("ITN Protection %") +
    ylab("Incidence per 1000") ,
  ggplot(final, aes(x = IRSprotn, y = inc1k, colour = Region, group = Region)) +
    geom_smooth() +
    xlab("IRS Protection %") +
    ylab("Incidence per 1000") ,
  labels = c("A","B") ,
  common.legend = TRUE, legend = "bottom"
)


# Doesn't seem like there are protection benefits so far... based on the protection by  IRS or ITN

#### 4 Table TIME  ####

# Things that could be useful to describe...
# 1. Number of districts covered by IRS, # of districts covered by ITN and IRS and plot that over time...


# Coverage of ITN 
md_yr <- final %>%
  select(DISTCODE, Epiyear, ITNprotn, IRSprotn) %>%
  group_by(DISTCODE, Epiyear) %>%
  summarize_all(funs(mean)) 

poly2 <- poly1
poly2 <- broom::tidy(poly2)
poly2$DISTCODE <- as.numeric(poly2$id)
poly2 <- left_join(poly2, md_yr)


# Plot ITN protection 
ggplot(data = poly2 %>%
         filter(Epiyear == 2012), # the input data
       aes(x = long, y = lat, fill = ITNprotn, group = group)) + # define variables
  geom_polygon() + # plot the districts
  geom_path(colour="black", lwd=0.05) + # district borders
  coord_equal() + # fixed x and y scales
  scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", # colors
                       midpoint = 0.5, name = "Protection") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks


poly1@data <- left_join(poly1@data, aggregate(ITNprot ~ DISTCODE, FUN = sum, data = final))
poly1@data <- left_join(poly1@data, aggregate(inc1k ~ DISTCODE, FUN = mean, data = final))
qtm(poly1,'cases') # Sum of all cases over 2010-2017 overall cases...
qtm(poly1,'inc1k') # average cases over 2010 to 2017 


# 2. 



#1.1 Plot 2016 total rainfall wk 1
spplot(SpatialPolygonsDataFrame(poly1,
                                column_to_rownames(as.data.frame(final %>%
                                                                   subset(Epiyear == 2016 & Epiweek == 1) %>%
                                                                   mutate(rowname = DISTCODE)))),
       "raintot", main = "Rainfall total", sub = "2016 week 1")

#1.1 Plot 2016 total cases wk 1
spplot(SpatialPolygonsDataFrame(poly1,
                                column_to_rownames(as.data.frame(final %>%
                                                                   subset(Epiyear == 2016 & Epiweek == 1) %>%
                                                                   mutate(rowname = DISTCODE)))),
       "cases", main = "Casees Total", sub = "2016 week 1")


#1.2 Multiple plots of different variables for 2016 wk 1
rainPal <- brewer.pal(n = 6, name = "YlGnBu") # palette 
spplot(SpatialPolygonsDataFrame(poly1,
                                column_to_rownames(as.data.frame(final %>%
                                                                   subset(Epiyear == 2016 & Epiweek == 1) %>%
                                                                   mutate(rowname = DISTCODE)))), 
       c("tavg", "raintot", "cases"), 
       names.attr = c("Ave temp", "Total rain","Malaria Cases"), 
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Mozambique Malaria Cases and Weather, Wk 1, 2016", 
       as.table = TRUE, 
       cuts=5, 
       col.regions = rainPal)


# Using ggplot to get multiple years and wks...
