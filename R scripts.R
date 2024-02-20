# https://docs.ropensci.org/rgbif/articles/getting_occurrence_data.html
#Retrieve occurrence data from GBIF
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download
library(taxize) # for get_gbifid
library(dplyr)

# fill in your gbif.org credentials 
user <- "" 
pwd <- "" 
email <- ""

name_backbone("Animalia") # get the taxon id information

large_wkt <- "POLYGON ((-5.316172 36.597889, -56.637537 6.664608, -76.673961 8.754795, -86.516414 12.21118, -100.225546 21.943046, -100.225546 33.431441, -81.946704 36.879621, -71.392652 52.05249, -73.853266 60.06484, -80.88359 65.802776, -74.556298 69.778952, 21.407625 69.411242, 19.298527 54.162434, -5.316172 36.597889))"
occ_download(
  pred_within(large_wkt),
  pred_gte("depth", 0),
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN"))),
  pred("taxonKey", 1),
  pred_lt("coordinateUncertaintyInMeters",100000),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
#   Check status with
occ_download_wait('')
# After it finishes, use
data_GBIF <- occ_download_get('') %>%
  occ_download_import()

# Remove the point data out of the shape file polygon of the study area
library(sf)
library(sp)
study_area <- st_read("C150_NA_review_area.shp") %>% #add the shape file polygon
  sf::st_as_sf()
#plot(study_area)
data_sf <- data_GBIF[, c("scientificName", "gbifID", "datasetKey", "depth", "decimalLatitude", "decimalLongitude")]
data_sf <- data_sf %>% st_as_sf(coords = c('decimalLongitude','decimalLatitude'))
st_crs(data_sf) = 4326
sf_use_s2(FALSE) # to switch of Spherical geometry (s2)
out <- st_intersection(data_sf, study_area)
library(magrittr) #assign coordinates to the dataframe
df <- out %>%
  dplyr::mutate(decimalLongitude = sf::st_coordinates(.)[,1],
                decimalLatitude = sf::st_coordinates(.)[,2])
dfg <- st_drop_geometry(df) # this is to drop the geometry column to be able to run the data_on_land

#library(dplyr)
#data_GBIF_all <- data_GBIF_taxmatch %>% mutate(across(c('decimalLatitude'), substr, 3, nchar(decimalLatitude))) # remove the first two extra charterers from column decimalLatitude
#write.csv(data_GBIF_all,'data_GBIF_all.csv')

# data cleaning
# data cleaning using obistools
#install.packages("devtools")
#devtools::install_github("iobis/obistools")

library("obistools")
#Check points on land
check_onland(dfg)
data_on_land <- check_onland(dfg, report = TRUE, buffer = 100000) # plot records on land with 100 meter buffer
print(data_on_land, n=1200)
plot_map_leaflet(dfg[data_on_land$row,], popup = "id")
#Plot points on a map
plot_map(dfg, zoom = TRUE)
#data_merged_clean <- dfg[-1 * data_on_land$row,] #Remove the points on land

#Check depth using obistools
plot_map(check_depth(dfg, depthmargin = 50), zoom = TRUE)
report <- check_depth(data_merged_clean, report=T, depthmargin = 50)
head(report)# as only mindepth is missing we do not need to do anything

#taxonmatch with WoRMS
names <- (dfg$scientificName)
match_taxa(names)# all taxa matched and all matched with worms, click the info to get the unmatched list and save it as Taxmatch, then match this with WoRMS and delete the unaccpted species
#taxmatch_matched <- read.csv('taxmatch_matched.csv')
data_GBIF_taxmatch <- anti_join(dfg, taxmatch_matched, by = "scientificName") # Removed the unaccepted non matched WoRMS species
#write.csv(data_GBIF_taxmatch,'data_GBIF_taxmatch.csv')

#Load OBIS data
data_OBIS_Benthic <- read.csv("Benthic_OBIS.csv") # The OBIS dataset were prepared by Amelia and she has sent them to me
data_OBIS_Pelagic <- read.csv("Pelagic_OBIS.csv")

# Filter OBIS data
data_OBIS_Benthic_fil <- data_OBIS_Benthic %>%
  dplyr::select(scientificName, dataset_id, decimalLatitude, decimalLongitude, obis_record_depth) %>%
  mutate(
    decimalLongitude = round(decimalLongitude, 3),
    decimalLatitude = round(decimalLatitude, 3)
  )

data_OBIS_Pelagic_fil <- data_OBIS_Pelagic %>%
  dplyr::select(scientificName, dataset_id, decimalLatitude, decimalLongitude, obis_record_depth) %>%
  mutate(
    decimalLongitude = round(decimalLongitude, 3),
    decimalLatitude = round(decimalLatitude, 3)
  )

data_GBIF_fil <- data_GBIF_taxmatch %>%
  dplyr::select(scientificName, datasetKey, decimalLatitude, decimalLongitude, depth) %>%
  mutate(
    decimalLongitude = round(decimalLongitude, 3),
    decimalLatitude = round(decimalLatitude, 3)
  )

# rename columns "Latitude" & "Longitude" to merge the data 
colnames(data_OBIS_Benthic_fil)[5] <- "depth"
colnames(data_OBIS_Pelagic_fil)[5] <- "depth" 
colnames(data_GBIF_fil)[2] <- "dataset_id" 

# Merge OBIS, GBIF, and integrated datasets and remove the duplicates by function distinct 
NAtlantic_data_merged <- bind_rows(data_OBIS_Benthic_fil, data_OBIS_Pelagic_fil, data_GBIF_fil) %>%
  distinct()
#write.csv(NAtlantic_data_merged, 'NAtlantic_data_merged.csv')



