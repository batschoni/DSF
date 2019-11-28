#install.packages("gender")
#install.packages("genderdata", repos = "http://packages.ropensci.org", type = "source")
library("tidyverse")
library("readr")
library("gender")
rm(list = ls())

####
# Inspecting Inspections
inspections <- read.csv("https://data.ny.gov/api/views/d6dy-3h7r/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)
inspections <- as_tibble(inspections)
inspectionsSave <- inspections
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
# To merge with ny17Cen both dataframes have to have a column with the same data
# we do not have this, to create it, we first create a third dataframe AddTrac, that contains the data of both the Addresses as well as the corresponding Census Tract IDs
# getting Data from official geocoding services
#https://geocoding.geo.census.gov/geocoder/geographies/addressbatch?form
#See this pdf to understand how geocoding works in general and how to use the bathc geocoding in particular: https://www2.census.gov/geo/pdfs/maps-data/data/FAQ_for_Census_Bureau_Public_Geocoder.pdf

geocoderIn <- inspections[ ,c("Street", "City", "State.Code", "Zip.Code")]
geocoderIn1 <- geocoderIn[1:9000, ]
write.csv(geocoderIn1, "geocoderIn1.csv")
geocoderIn2 <- geocoderIn[9001:18999, ]
write.csv(geocoderIn2, "geocoderIn2.csv")
geocoderIn3 <- geocoderIn[19000:nrow(geocoderIn), ]
write.csv(geocoderIn3, "geocoderIn3.csv")
geocoderInTest <- geocoderIn[1:10, ]
write.csv(geocoderInTest, "geocoderInTest.csv")


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
#lin1geo etc. still need to be added somehow (do they?)
CC <- complete.cases(AddTrac$`10`)
AddTrac <- AddTrac[CC, ]


#Since the TractId geocode contains placeholder zeros, which are not displayed in the batch goecoding output, those zeros have to be added, to make merging possible
f1 = function(x){
  if(nchar(x) == 1){paste("00", x, sep = "")}
        else if (nchar(x) == 2){paste("0", x, sep = "")}
        else {paste(x, "", sep = "")}
}

#min(nchar(AddTrac$`11`))

f2 = function(x){
  if(nchar(x) == 3){paste("000", x, sep = "")}
  else if (nchar(x) == 4){paste( "00",x, sep = "")}
  else if (nchar(x) == 5){paste( "0",x, sep = "")}
  else {paste(x, "", sep = "")}
}

AddTracTest <- AddTrac[1:100,]
fT <- function(x){paste(x, "0", sep = "")}
test <- lapply(AddTracTest$`10`, fT)   
AddTracTest$`10` <- lapply(AddTracTest$`10`, f1)
AddTrac$`10` <- lapply(AddTrac$`10`, f1)
AddTrac$`11` <- lapply(AddTrac$`11`, f2)


AddTrac <- unite(AddTrac, TractId, c("9","10","11"), sep = "")
colnames(AddTrac)[1:2] <- c("Numbers","Address")
AddTrac <- AddTrac[,c("Address","TractId")]

#######
# Match AddTrac with Inspections to have the TractId numbers included in the Inspections Dataframe
inspections <- inspectionsSave
inspections$County <- toupper(inspections$County)
inspections <- unite(inspections, Address , c(Street, County, State.Code, Zip.Code), sep = ", ", remove = FALSE)
inspectionsTrac <- merge(inspections, AddTrac, by = "Address")

#####
#Match InspectionsTrac with democraphic Data by Census data per census tract
inspectionsCen <- merge(inspectionsTrac, ny17Cen, by.x = "TractId", by.y = "TractId per CenTrac")

###
# Match all demographic data
ny17county$`County per County` <- toupper(ny17county$`County per County`)
inspectionsDem <- merge(inspectionsCen, ny17county, by.x = "County", by.y = "County per County")

filter(inspectionsDem, c("NEW YOKR", "KINGS COUNTY", "QUEENS", "RICHMOND"))

write.csv(inspectionsDem, file = gzfile("C:/Users/andre/Documents/HSG/W-DS/inspectionsDem.cvs.gz"))
INSPECTIONS <- read.csv("~/HSG/W-DS/inspectionsDem.cvs.gz")

