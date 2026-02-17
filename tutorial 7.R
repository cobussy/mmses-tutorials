#tutorial 7

#install.packages("icesDatras") 
install.packages("icesDatras")
install.packages("sf")
install.packages("rnaturalearth")
library(icesDatras)     # Needed to extract RV survey data
library(sf)             # For GIS analyses
library(ggplot2)        # For plotting
library(rnaturalearth)  # For getting countries/coastline data

# List all available surveys
surveys <- getSurveyList()
print(surveys)

# List available years for a survey
years <- getSurveyYearList("EVHOE")
print(years)

#Another function called getSurveyYearQuarterList(),
#takes a survey name and a year, and returns the quarter(s) 
#of the year that data are available. 
#Packing it in an sapply() function 
sapply(years, function(x) {getSurveyYearQuarterList("EVHOE",x)})

# Fetch haul and species data
hh <- getHHdata(survey = "EVHOE", year = 2020, quarter = 4)
hl <- getHLdata(survey = "EVHOE", year = 2020, quarter = 4)

#First 2 records for each of these data sets.
head(hh,2)
head(hl,2)

#Making unsinque identifiers for merging data sets
hh$uniqueID <- paste(hh$Country, hh$HaulNo)
hl$uniqueID <- paste(hl$Country, hl$HaulNo)

# Keep only data for  valid_Aphia 127419 (Capros aper, "boarfish"), and keep only limited set of columns (unique haul ID and total number caught) 
spec_hl <- subset(hl, Valid_Aphia == 127419, select = c(uniqueID, TotalNo))

# Remove any duplicated unique IDs. The TotalNo column is then the total number of boarfish caught per haul.
# Also keep only 
spec_hl_uniqueID <- spec_hl[!duplicated(spec_hl$uniqueID),]

# Now merge the count data with the haul information in hh, using HaulNo as unique identifier in both.
# In passing, select only relevant columns (Haul number and location: ShootLat, ShootLong) from hh records
# The all = T argument ensures that we keep all hauls, even those where no boarfish was 
# caught (and hence, for which there are no hl records).
final_spec <- merge(spec_hl_uniqueID, hh[,c("uniqueID","ShootLat","ShootLong")], by ="uniqueID", all=T)

# As a final step, we replace the NAs in TotalNo with 0: that is to say, if there was no hl info for 
# should be interpreted as a catch without boarfish, and thee zeroes should be explicittly included
final_spec$TotalNo[is.na(final_spec$TotalNo)] <- 0

# make into simple feature
final_spec_sf <- st_as_sf(final_spec, coords = c("ShootLong", "ShootLat"), crs = 4326)

# Get coastline
world <- ne_countries(scale = "medium", returnclass = "sf")

#plot
ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey60") +
  geom_sf(data = final_spec_sf,  aes(size = TotalNo)) +
  #scale_size(range = c(2, 10)) +
  coord_sf(xlim = c(-12, 2), ylim = c(42, 53)) +
  theme_minimal() +
  labs(size = "Total catch for species per haul")