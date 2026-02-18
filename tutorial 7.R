#tutorial 7

#install all packages necessary for exercises 
install.packages("icesDatras")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
library(icesDatras)     # Needed to extract RV survey data
library(sf)             # For GIS analyses
library(ggplot2)        # For plotting
library(rnaturalearth)  # For getting countries/coastline data
library(rnaturalearthdata)

# List all available surveys
surveys <- getSurveyList()
print(surveys)

#exercise 2.1 

# List available years for a survey
years <- getSurveyYearList("NS-IBTS")
print(years)

# Packing it in an sapply() function 
sapply(years, function(x) {getSurveyYearQuarterList("NS-IBTS",x)})

# Fetch haul and species data
hh <- getHHdata(survey = "NS-IBTS", year = 2025, quarter = 1)
hl <- getHLdata(survey = "NS-IBTS", year = 2025, quarter = 1)

# First 2 records for each of these data sets.
head(hh,2)
head(hl,2)

# Making unsinque identifiers for merging data sets
hh$uniqueID <- paste(hh$Country, hh$HaulNo)
hl$uniqueID <- paste(hl$Country, hl$HaulNo)

# Keep only data for  valid_Aphia 105814 (Scyliorhinus canicula, "dogfish"), and keep only limited set of columns (unique haul ID and total number caught) 
spec_hl <- subset(hl, Valid_Aphia == 105814, select = c(uniqueID, TotalNo))

# Remove any duplicated unique IDs. The TotalNo column is then the total number of dogfish caught per haul.
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
  coord_sf(xlim = c(-4, 9), ylim = c(51, 62)) +
  theme_minimal() +
  labs(size = "Total catch for species per haul")

# example 2 

# packages and libraries example 2

install.packages("emodnet.wfs", repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org"))
library(emodnet.wfs)

# services emodnet wfs
services <- emodnet_wfs()
services

# connecting individual services 
wfs_bio <- emodnet_init_wfs_client(service = "biology")

# service layers info 
bio_layers <- emodnet_get_wfs_info(service = "biology")
bio_layers$layer_name

# simplifying list for posidonia
posidonia <- emodnet_get_layers(wfs = wfs_bio, layers = "mediseh_posidonia_current_pnt", simplify = TRUE)

# This function takes the outer coordinates, of the posidonia data
bbox <- st_bbox(posidonia)

# Plot data
ggplot() +
  # land first (background)
  geom_sf(data = world, fill = "grey85", color = "grey60", linewidth = 0.2) +
  geom_sf(data = posidonia, color = "darkgreen", linewidth = 0.4) + 
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    crs = 4326,
    expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Posidonia seagrass distribution",
    subtitle = "EMODnet Biology (WFS)",
    x = "Longitude",
    y = "Latitude"
  )

# wfs substrates 
wfs_substrates <- emodnet_init_wfs_client(service = "geology_seabed_substrate_maps")
substrate_layers <- emodnet_get_wfs_info(service = "geology_seabed_substrate_maps")
substrate_layers$layer_name

# substrating scale
substrate <- emodnet_get_layers(wfs = wfs_substrates,
                                layers = c("seabed_substrate_250k"),
                                simplify = TRUE,
                                cql_filter = "BBOX(geom, 0, 50, 4, 54,'EPSG:4326') ")

#plot substrates 
ggplot() +
  # land first (background)
  geom_sf(data = world, fill = "grey85", color = "grey60", linewidth = 0.2) +
  geom_sf(data = substrate , aes(fill = folk_16cl_txt), linewidth = 0.4) +
  coord_sf(crs = 4326, expand = FALSE,
           xlim = c(1.5, 6),
           ylim = c(51.2, 53.5)) +
  theme_minimal() +
  labs(title = "Substrate map",
       subtitle = "EMODnet Geology (WFS)",
       x = "Longitude",
       y = "Latitude")

# exercise 2.2 
# making map of Krk

#services emodnet wfs
services <- emodnet_wfs()
services

# connecting individual services 
wfs_bio <- emodnet_init_wfs_client(service = "biology")

# service layers info 
bio_layers <- emodnet_get_wfs_info(service = "biology")
bio_layers$layer_name

# simplifying list for posidonia
posidonia <- emodnet_get_layers(wfs = wfs_bio, layers = "mediseh_posidonia_current_pnt", simplify = TRUE)

# This function takes the outer coordinates, of the posidonia data
bbox <- st_bbox(posidonia)

# Plot data
ggplot() +
  # land first (background)
  geom_sf(data = world, fill = "grey85", color = "grey60", linewidth = 0.2) +
  geom_sf(data = posidonia, color = "darkgreen", linewidth = 0.4) + 
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    crs = 4326,
    expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Posidonia seagrass distribution",
    subtitle = "EMODnet Biology (WFS)",
    x = "Longitude",
    y = "Latitude"
  )

# wfs substrates 
wfs_substrates <- emodnet_init_wfs_client(service = "geology_seabed_substrate_maps")
substrate_layers <- emodnet_get_wfs_info(service = "geology_seabed_substrate_maps")
substrate_layers$layer_name

# substrating scale 44.9N,14.3E; 45.3N;14.9E
substrate <- emodnet_get_layers(wfs = wfs_substrates,
                                layers = c("seabed_substrate_250k"),
                                simplify = TRUE,
                                cql_filter = "BBOX(geom, 14.3, 44.9, 14.9, 45.3,'EPSG:4326') ")

#plot substrates 
ggplot() +
  # land first (background)
  geom_sf(data = world, fill = "grey85", color = "grey60", linewidth = 0.2) +
  geom_sf(data = substrate , aes(fill = folk_7cl_txt), linewidth = 0.4) +
  coord_sf(crs = 4326, expand = FALSE,
           xlim = c(14.3, 14.9),
           ylim = c(44.9, 45.3)) +
  theme_minimal() +
  labs(title = "Substrate map",
       subtitle = "EMODnet Geology (WFS)",
       x = "Longitude",
       y = "Latitude")

# example 3 
# packages and libraries example 3
install.packages("robis") #run once

#start from here after installing robis
library (robis)

# Get occurrence data from OBIS using occurrence()
catshark <- occurrence(
  scientificname = "Scyliorhinus canicula",
  startdate = as.Date("2017-01-1")
)

#check collectioncode in catshark object 
catshark



