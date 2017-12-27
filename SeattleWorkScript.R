###Seattle Population Data,Census Tract.
library(dplyr)
library(purrr)
library(tidycensus)
library(tigris)
library(sf)
library(rgdal)

## Police Data for Seattle Final!! #####

PoliceData <-read.csv(file.choose())


View(PoliceData)
###############   TOTAL POPULATION BY CENSUS TRACT,census block, and block group FOR KING COUNTY ####################################

totalpop <- get_acs(geography = "tract", variables = "B01003_001",  state = "WA",county = "king county", geometry = TRUE)
View(totalpop)

######################################## ---- xx ----- xx ----- #############################################






################################# TOTAL POPULATION BY BLOCK GROUP ######################################################

totalpop2 <- get_acs(geography = "block group", variables = "B01003_001",  state = "WA",county = "king county", geometry = TRUE)
View(totalpop2)

####################################---- XX    --    XX ##############################################################




View(SeattleCensusBlock)

help(geograph). 

geometrydata <- data.frame(totalpop$geometry)

LongLat <- c(PoliceData$Longitude)

### Metropolitan Seattle Area based on function below. 
popse <- core_based_statistical_areas(cb = TRUE) %>%
  filter(GEOID %in% c("42660")) %>%
  select(popse_name = NAME)                               

################## -- xx --- xxx #########################
View(popse)

wcpop <-st_join(totalpop,popse, join = st_within, left = FALSE) 


View(wcpop)


#####Rough Codes###########

#by block. 
SeattleCensusBlock <- get_decennial(geography = "block", variables = "P0010001", 
                                    state = "WA", county = "King", geometry = TRUE)
##################################################### -- XX -- XX ######################################################











#########acs package functions
totaltracts <- geo.make(state="WA", county="King", tract="*")
geo.list(totaltracts)

api.key.install()

acsPopulation <- acs.fetch(endyear = 2010, span = 5 , totaltracts, variables ="B01003_001",col.names=c("Total","Male","Female"))

#### -- XX-- XXX 










us <- unique(fips_codes$state)[48]


summary(PoliceData)
View(PoliceData$At.Scene.Time)
tail(At.Scene.Time)
View(PoliceData)

PoliceData %>% right_join(Event.Clearance.Group,At.Scene.Time)
v
##Removing Numbers 
View(PoliceData)

PoliceData %>% substr

###acs table cleaning function to merge ###############

Census.Final <- substr(totalpop$GEOID,6,nchar(totalpop))

UpdatedPoliceData <- totalpop %>% mutate(Census.Final)

nrow(UpdatedPoliceData)

#######---x----------x----############################
####### Tuesday October 17th, 2017. CensusTractID can be matched with CensusTractFinal1. #########

FinalPoliceReport <- mutate(UpdatedPoliceData[c(1,7,2,3,4,5,6)]) 

View(FinalPoliceReport)
####### --      x   ---    x         #########


count(UpdatedPoliceData,GEOID)
View(UpdatedPoliceData)

help(substr)

help(right_join)

View(totalpop)

View(PoliceData)


GeoIDrem

View(GeoIDrem)

s <- sub("().*","",PoliceData$Census.Tract)
s

View(FinalPoliceReport)


DecimalR <- sub("(.).*", "", PoliceData$Census.Tract)

TractPopulationSeattle <- inner_join(FinalPoliceReport,PoliceData,by = "Census.Final")

SeattleFinalPolice <- write.csv(newdataset,file = "SeattleFinal.csv")


View(TractPopulationSeattle)



View(newdataset)


View(totalpop)


geometrylist <- totalpop %>% select(geometry)

View(geometrylist)


geometrytract <- write.csv(geometrylist,file = "geometrytract.csv")


View(geometrylist) 

###Rough Codes

if(!require(pacman)){install.packages("pacman");
  library(pacman)}
p_load(ggplot2, tidycensus, dplyr)

if(!require(pacman)){install.packages("pacman");
  library(pacman)}
p_load(sf, tigris, viridis, ggthemes, ggplot2,
       tidycensus, stringr, dplyr)

options(tigris_class = "sf", tigris_use_cache = TRUE)

library(ggplot2)

p1 <- ggplot() + geom_sf(data = totalpop, aes(fill = estimate)) +coord_sf(datum = NA) +theme(plot.title = element_text(size = 16,
                                  face = "bold", margin = margin(b=10))) +
  theme(plot.subtitle = element_text(size = 14,
                                     margin = margin(b = -20))) +
  theme(plot.caption = element_text(size = 9,
                                    margin = margin(t = -15), hjust = 0)) +
  scale_fill_viridis(labels = scales::dollar,
                     name = "MHI Estimate") +
  labs(caption = "Source: US Census Bureau ACS (2011-2015)",
       title = "Median Household Income for PDX Metro\n at the census tract level",
       subtitle = "An R 'sf' Example") + theme_minimal()


#### Codes for Splitting the Time from Date ########


Hours <- format(as.POSIXct(strptime(PoliceData$Event.Clearance.Date,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")

Hours

Dates <- format(as.POSIXct(strptime(PoliceData$Event.Clearance.Date,"%m/%d/%d %H:%M",tz="")) ,format = "%m/%d/%y")

Dates

PoliceData$Dates <- Dates
PoliceData$Hours <- Hours

######## End of codes for splitting ########


View(Dates)
Subsetd <- subset(PoliceData, select = c(1,2,4,5))
View(Subsetd)
### Rough codes for splitting ###


View(PoliceData)

