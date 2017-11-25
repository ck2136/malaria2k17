##########################
# Data Management File
# Created by: CK
# Created on: 10/24/2017
# Modified by: CK
# Modified on: 10/24/2017
##########################

### Objective ####
# This code is used to import, clean, merge, and manipulate data for later analysis


### 1. Import necessary libraries ####
rm(list=ls())
library(haven) # for loading data
library(stringr) # for manipulating 
library(readr) # for loading data
library(EpiWeek) # for creating epiweek variables
library(tidyverse) # for additional data transformation
library(rgdal)
library(sp)
library(maptools) 

#### 1.1 Importing Incidence Data ####

incidence <-  read_csv("/home/ck1/Documents/PythonR/Project/data/incidence.csv")

#### 1.2 Importing interventions Data ####
intervention <-  read_csv("/home/ck1/Documents/PythonR/Project/data/intervention.csv")

### 1.3 Importing Weather Data ####
# Load the url
url <- "http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/"

# Parse html file to extract the hyperlink with the text files
html <- paste(readLines(url), collapse="\n")
matched <- str_match_all(html, "<a href=\"(.*?)\"")
temp <- matched[[1]][,2]
txtfiles <- temp[grep(".txt", temp)]

# Extract district name for later use
distnam <- gsub("_fldas_daily_20100101-20170724.txt", "", x = txtfiles)

# create code that will paste the url into the file 
txtfiles <- sapply(txtfiles, FUN =  function(x) paste0("http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/",x))

# reads in the whole data now and rbind them together and put them in the weatherdata dataframe
library(data.table)
weatherdata = do.call(list, lapply(txtfiles, fread))
weatherdata <- rbindlist(weatherdata, use.names = TRUE, fill =TRUE, idcol = "District")

# Remove the last part of the url
weatherdata$District <-gsub("_fldas_daily_20100101-20170724.txt", "", x = weatherdata$District)

# save the province name
weatherdata$Province <- unlist(lapply(strsplit(weatherdata$District ,split="_"), function(x) tail(x, 1)))

# replace district name with just district name
weatherdata$District<- gsub("\\s*\\w*$","" ,gsub("_", " ", weatherdata$District))
weatherdata$District2 <- weatherdata$District

# sort alphabetically by district to get district code
weatherdata <- weatherdata[order(weatherdata$District2),]
incidence <- incidence[order(incidence$District),]

distcodes <- incidence %>%
  group_by(District, DISTCODE) %>%
  summarise()

weather_dist <- weatherdata %>%
  group_by(District2) %>%
  summarise()

# once district code determined 
weather_dist <- data.frame(weather_dist, distcodes[,2])
weatherdata <- merge(weatherdata, weather_dist, by="District2")


# add column names
colnames(weatherdata) <- c("District2",  "District", "year", "mo", "day", "raint", "tavg", "rh", "sd", "psfc", "Province", "DISTCODE")

# Create Epiweek and Epiyear variable 
weatherdata$Epiweek <- dateToEpiweek(paste0(weatherdata$year,"-",weatherdata$mo,"-",weatherdata$day))$weekno
weatherdata$Epiyear <- dateToEpiweek(paste0(weatherdata$year,"-",weatherdata$mo,"-",weatherdata$day))$year


weatherdataepi <- weatherdata %>%
  filter(Epiyear >= 2010) %>%
  group_by(DISTCODE, Epiyear, Epiweek) %>%
  summarise(tavg = mean(tavg), raintot = sum(raint), rh = mean(rh), sd = mean(sd), psfc = mean(psfc))

# Noramlize the values
weatherdataepi$normRain <- (weatherdataepi$raintot-mean(weatherdataepi$raintot))/sd(weatherdataepi$raintot)
weatherdataepi$normTavg <- (weatherdataepi$tavg-mean(weatherdataepi$tavg))/sd(weatherdataepi$tavg)
weatherdataepi$normrh <- (weatherdataepi$rh-mean(weatherdataepi$rh))/sd(weatherdataepi$rh)

rm(weather_dist, weatherdata, distcodes)

#### 1.4 Checking Data ####
# Check missing values 
lapply(incidence, function(x) table(is.na(x))) # missing cases and u5 total
lapply(intervention, function(x) table(is.na(x))) # missing 
lapply(weatherdataepi, function(x) table(is.na(x))) # no data missing in weather
lapply(temp, function(x) table(is.na(x))) # no data missing in weather


#### 1.5 Merge Data ####

# 1.5.1 Merge Weatherdata W Incidence Data
wdei <- merge(weatherdataepi, incidence, by = c("DISTCODE", "Epiyear", "Epiweek"), all = TRUE)

rm(weatherdataepi, incidence, distnam, html, temp, txtfiles, url, matched)


# 1.5.2 Merge Weatherdata & Incidence Data With Intervention Data 

# Intervention dataset merge IRS 75% after 6 months... constant decrease iTN 60% protection after 24 months.. create protection variable starting at 1 then decreasing constantly by weeks. Assume 4 weeks in a month.

#ITN and IRS dates for districts change to POSITX format for easy calculation

# Create IRS protection variable
temp2 <- merge(wdei, 
               intervention %>% # here we only select the IRS year first 
                 select(DISTCODE, IRSyear, IRSepiWeek) %>% # select only IRS 
                 filter(!is.na(IRSyear) & IRSyear > 2009) %>% #Select for Epiyear > 2009
                 rename(Epiyear =  IRSyear, Epiweek = IRSepiWeek) %>% # Rename to match
                 mutate(IRSprot = ifelse(!is.na(Epiyear), 1, NA)), # Create protection variable 
               by = c("DISTCODE","Epiyear","Epiweek"), all = TRUE)

# Create ITN protection variable 
temp3 <- merge(temp2, 
               intervention %>% # here we only select the ITN year first 
                 select(DISTCODE, ITNyear, ITNepiWeek) %>% # select only ITN 
                 filter(!is.na(ITNyear) & ITNyear > 2009) %>% #Select for Epiyear > 2009
                 rename(Epiyear =  ITNyear, Epiweek = ITNepiWeek) %>% 
                 mutate(ITNprot = ifelse(!is.na(Epiyear), 1, NA)), # Create ITN protection variable
               by = c("DISTCODE","Epiyear","Epiweek"), all = TRUE)
temp3$dates <- epiweekToDate(temp3$Epiyear, temp3$Epiweek)[[1]] # Create date column to check dates 


# Now create two treatment variables that will have the actual protection for IRS and ITN

# Calculation for proportionality constant of decaying protection for IRS and ITN

# IRS 0.75 = exp(-k * (52.1429/2))
# ITN 0.6 = exp(-k * 104) 

IRSmult = exp(-(2*log(0.75)/-(365/7)))
ITNmult = exp(-(log(0.60)/-(365/7 * 2)))  

# Here is the looped version... basically looping over all the district codes and seeing if they have a 1 or not if they do have a 1 then given the district the next value will be a (proportionality constant) multiple of the previous value (i.e. lagged) this will take some time...

# for(i in 1:nrow(temp3)){
#   for(dist in unique(temp3$DISTCODE)) {
#     if(!is.na(temp3$IRSprot) && temp3$DISTCODE == dist) {
#       temp3[(i+1),'IRSprot'] = temp3[i,'IRSprot'] * IRSmult
#     }
#     if(!is.na(temp3$ITNprot) && temp3$DISTCODE == dist) {
#       temp3[(i+1),'ITNprot'] = temp3[i,'ITNprot'] * ITNmult
#     }
#   }
# }

# or here is another way... first create 0's for the NA's 
temp3$IRSprot[is.na(temp3$IRSprot)] <- 0
temp3$ITNprot[is.na(temp3$ITNprot)] <- 0


# This is the code to get the diminishing protection each week...
temp4 <- temp3 %>% 
  select(-dates) %>%
  group_by(DISTCODE) %>%
  mutate(IRSprotn= ifelse(IRSprot == 1, 1, lag(cumsum(IRSprot),k=1, default = 0)),
         IRSprotn= ifelse(IRSprot == 1 & IRSprotn == 1, 1, ifelse(IRSprotn == 0, 0,IRSmult^(cumsum(IRSprotn)))),
         ITNprotn= ifelse(ITNprot == 1, 1, lag(cumsum(ITNprot),k=1, default = 0)),
         ITNprotn= ifelse(ITNprot == 1 & ITNprotn == 1, 1, ifelse(ITNprotn == 0, 0,ITNmult^(cumsum(ITNprotn))))
)
rm(temp2, temp3) # remove previous datas

#### 1.6 Check Data ####
lapply(temp4, function(x) table(is.na(x)))

# So far missing SQKM, Province, Region, u5total, and cases
# check size of data
object.size(temp4) # ~ 10 mb

#### 1.7 Import Spatial Data ####
poly1 <- readShapePoly('/home/ck1/Documents/PythonR/Project/data/Moz_admin2.shp', IDvar="DISTCODE")

#If we decide to run each polygon stuff.. 8 years (2010-2017) * 52 weeks/yr * 9 mg/object = 3744 mb or roughly 3.8 gb's probably a little heavy on this machine...


#### 1.8 Missing Data Check and irregularities ####

lapply(temp4, function(x) table(is.na(x)))
# 531 observations have missing cases and u5total ... may need to do imputation may not...
# 7108 have missing region.. seeing that there are districts that have region that just have NA in these we just need to fill in the Regions, Province, and SQKM 
# 

temp5 <- temp4

# create new data frame that will be used to merge back in to the original data 
y <- temp4 %>%
  filter(!is.na(Region)) %>%
  select(DISTCODE, Region, Province, SQKM) %>%
  distinct(Region, Province, SQKM)

temp5 <-temp4 %>%
  left_join(y, by = "DISTCODE") %>%
  mutate(Region = coalesce(Region.x, Region.y),
         Province = coalesce(Province.x, Province.y),
         SQKM = coalesce(SQKM.x, SQKM.y)) %>%
  select(-Region.x, -Region.y, -SQKM.x, -SQKM.y, -Province.x, -Province.y)

#Check which one is the Region, Province, and SQKM that is missing
#DISTCODE = 1101 DISTRINCT = CIDADE DE MAPUTO google and add back in
#Apprently CIDADE de MAPUTO is considered it's own province on wikipedia... but Dr. Colborn said to keep it as MAPUTO
temp5[temp5$DISTCODE == 1101,]$Region <- "Coastal"
temp5[temp5$DISTCODE == 1101,]$Province <- "MAPUTO"
temp5[temp5$DISTCODE == 1101,]$SQKM <- 346.77


unique(temp5$Province)

# check NA on cases
temp5[is.na(temp5$cases),] %>%
  distinct(Epiyear) # seems like 1101 no recorded cases during each year and 320 recording lost at 15',16',17

# Check summary
summary(temp5)
# District codes seem alright
# Epi year and Week seem to follow a nice 1 to 53 weeks
# total rain of 693.310... not too bad
# Other values seem normal...

temp4 <- temp5
#### 1.9 Make incidence variable (per 1000) ####
temp4$inc1k <- (temp4$cases / temp4$u5total)*1000


#### 2 Make Lag Variables #### 
library(data.table)
final <- cbind(temp4, temp4[, shift(.SD, c(2,4,8), give.names = TRUE), .SDcols = c('tavg','raintot','rh','sd','psfc')])
final <- as.data.frame(final)
saveRDS(final, file = "~/Documents/PythonR/Project/data/final.rds")

rm(list=ls())
