library("tidyverse")
library("readr")
rm(list = ls())

####
# Inspecting Inspections
inspections <- read.csv("https://data.ny.gov/api/views/d6dy-3h7r/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)
inspections <- as_tibble(inspections)
sum(is.na(inspections$Zip.Code))
x <- length(unique(inspections$County))
   


#####
# Experimenting with dem. Datasets & modifying them
nycdBlockLoc <- read.csv("~/GitHub/DSF/data/nycd_census_block_loc.csv")
nycdBlockLoc <- as_tibble(nycdBlockLoc)
nycdCen <- read.csv("~/GitHub/DSF/data/nycd_nyc_census_tracts.csv")
nycdCen <- as_tibble(nycdCen)

#sf1 <- read.table("~/GitHub/DSF/data/sf1_dp_cncld_demoprofile.xlsx")

us17Cen <- read_csv("~/GitHub/DSF/data/K_us-census-demographic-data/acs2017_census_tract_data.csv")
# taken from: https://www.kaggle.com/muonneutrino/us-census-demographic-data
us17Cen <- as_tibble(us17Cen)
ny17Cen <- filter(us17Cen, State == "New York")
ny17Cen <- mutate(ny17Cen, percentageFemale = (TotalPop - Men)/TotalPop) #not necessary to add second category, since dataset strictly selected in either male or female

ny17CenNames <- names(ny17Cen)
ny17CenNames <- paste(ny17CenNames, "per CenTrac")
ny17Cen <- setNames(ny17Cen, ny17CenNames)

us17county <- read.csv("~/GitHub/DSF/data/K_us-census-demographic-data/acs2017_county_data.csv")
# taken from: https://www.kaggle.com/muonneutrino/us-census-demographic-data
us17county <- as_tibble(us17county)
ny17county <- filter(us17county, State == "New York")
ny17county <- mutate(ny17county,percentageFemale = (TotalPop - Men)/TotalPop) #not necessary to add second category, since dataset strictly selected in either male or female
 
ny17countyNames <- names(ny17county)
ny17countyNames <- paste(ny17countyNames, "per County")
ny17county <- setNames(ny17county, ny17countyNames)


#######
#Merging inspections with ny17county
class(inspections)==class(ny17county)
class(ny17county$`County per County`)
ny17county[,"County per County"] <- str_remove(ny17county$`County per County`, " County")
inspectionsCounty <- merge(inspections, ny17county, by.x = "County", by.y = "County per County")


########
# merging with ny17Cen
a <- filter(ny17Cen, TractId == 36081043900)

geocoderIn <- inspections[ ,c("Street", "City", "State.Code", "Zip.Code")]
geocoderIn1 <- geocoderIn[1:9000, ]
write.csv(geocoderIn1, "geocoderIn1.csv")
geocoderIn2 <- geocoderIn[9001:18999, ]
write.csv(geocoderIn2, "geocoderIn2.csv")
geocoderIn3 <- geocoderIn[19000:nrow(geocoderIn), ]
write.csv(geocoderIn3, "geocoderIn3.csv")
geocoderInTest <- geocoderIn[1:10, ]
write.csv(geocoderInTest, "geocoderInTest.csv")

#geocodeOuTest <- read.csv("~/GitHub/DSF/data/geocodeOut1.csv")
#geocodeOut1 <- read.delim("~/GitHub/DSF/data/geocodeOutTest.csv")

#geoOutTestNames <- read.table("~/GitHub/DSF/data/GeocodeOutTest.csv", nrow = 1, stringsAsFactors = FALSE, sep = ",")
#geoOutTestData <- read.table("~/GitHub/DSF/data/GeocodeOutTest.csv", skip = 1, stringsAsFactors = FALSE, sep = ",")
#DATA <- DATA[, 1:24]
#(DATA) <- NAMES 

geoOutTestData <- read_csv("~/GitHub/DSF/data/GeocodeOutTest2.csv")

geoOut1 <- read.csv("~/GitHub/DSF/data/geoOut1.csv", skip = 1)
geoOut2 <- read.csv("~/GitHub/DSF/data/geoOut2.csv", skip = 1)
geoOut3 <- read.csv("~/GitHub/DSF/data/geoOut3.csv", skip = 1)
line1geo <- as.vector(names(geoOut1))
line2geo <- names(geoOut2)
line3geo <- names(geoOut3)
names(geoOut1) <- c(1:ncol(geoOut1))
names(geoOut2) <- c(1:ncol(geoOut2))
names(geoOut3) <- c(1:ncol(geoOut3))
AddTrac <- rbind(geoOut1, geoOut2,geoOut3)
#lin1geo etc. still need to be added somehow

#b <- duplicated(AddTrac[2], )
#duplicated(AddTrac[2], )



AddTrac <- unite(AddTrac, TractId, c("9","10","11"))
AddTrac$TractId <- str_remove(AddTrac$TractId, "_")
AddTrac$TractId <- str_remove(AddTrac$TractId, "_") #yes thats necessary, removes pattern only once
colnames(AddTrac)[1] <- c("Numbers")


#######
# Match AddTrac with Inspections to have the TractId numbers included in the Inspections Dataframe

inspections <- mutate(inspections, Numbers = c(1:nrow(inspections)))
inspectionsTrac <- merge(inspections, AddTrac, by = "Numbers")

#####
#Match InspectionsTrac with democraphic Data by Census data per census tract
inspectionsCen <- merge(inspectionsTrac, ny17Cen, by.x = "TractId", by.y = "TractId per CenTrac")
