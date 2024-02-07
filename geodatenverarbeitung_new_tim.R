# Solarenergie- und -anlagenpotenzial Salzburgs - eine Analyse und tentative Machbarkeitsstudie
# Date: January 2024
# Author: Timothy Sung S

library(tmap)
library(raster)
library(terra)
library(stars)
library(sp)
library(sf)
library(tidyverse)
library(osmdata)
library(janitor)
library(leaflet)
library(leaflet.extras)
library(cluster)
library(factoextra)


# set working directory
setwd("~/Documents")

#### ----------- Data Download and CRS Check and Transformation ----------- ####
# read in solar potential data ----

solar_raw <- rast("offline_files/Solarpot/solarpot_31258.tif")

#solar_raw@crs
#hist(solar_raw, main="Distribution of elevation values", 
#     col= "purple", 
#     maxpixels=22000000)

#plot(solar_raw)

# download data on municipality borders and create state border polygon  ----
# download raw data on municipality borders 
gem_grenz_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/ESRI_SHAPE/Gemeindegrenzen.shp")

# create a polygon with the boundaries of Sazlburg state
bundesland <- gem_grenz_raw %>%
  mutate(merge_id = 1) %>% # generate IDs for grouping
  group_by(merge_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()


# read in data on land cover, combine layers and amend CRS ----
# lc_a_raw <- rast("offline_files/land_cover/ESA_WorldCover_10m_2021_v200_N45E012_Map.tif")
# lc_b_raw <- rast("offline_files/land_cover/ESA_WorldCover_10m_2021_v200_N48E012_Map.tif")

# combine raster layers
#lc_comb <- terra::merge(lc_a_raw, lc_b_raw)

# crop raster to extent of Salzburg bb (if needed - not necessary for below steps)
#gem_grenz_4326 <- st_transform(gem_grenz_raw, "EPSG:4326")# transform CRS to that of lc_comb
#lc_comb_sbg_bb <- terra::crop(lc_comb, gem_grenz_4326) # crop lc data to Salzburg bb

# mask raster to extent of Salzburg polygon
#bundesland_4326 <- st_transform(bundesland, "EPSG:4326") # transfrom Salzburg boundaries to EPSG: 4326
#lc_comb_sbg <- terra::mask(lc_comb, bundesland_4326) # mask lc data to Salzburg

# re-project lc_comb_sbg to working CRS
#lc_sbg <- project(lc_comb_sbg, "EPSG: 31258")

# write out raster to save time on processing
# f <- file.path("daten_zwischenablage", "lc_sbg.tif") # define file path
# writeRaster(lc_sbg, f, overwrite = T) # write out raster

# read in data on zoning ----
fl_wid_raw <- st_read("offline_files/Flaechenwidmung/Flaechenwidmung.shp")

# read in dsm ----
dgm <- rast("offline_files/dgm5m/dgm5m.asc")

# convert to working crs of EPSG: 31258 (if not already in it)
dgm <- project(dgm, "epsg:31258")

f <- file.path("daten_zwischenablage", "dgm.tif")

writeRaster(dgm, f, overwrite = T) # write out raster


# download data on electricity lines ----
strom_leitungen_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/DLM_2000_BAUTEN_20230912/BAU_2700_STROMLEITUNG_L.shp")

# check CRS
st_crs(strom_leitungen_raw) # CRS is 32155

# transform CRS
strom_leitungen_31258<- st_transform(strom_leitungen_raw, "EPSG: 31258")

# clip extent of strom_leitungen_31258 to Salzburg state

strom_leitungen_sbg <- st_intersection(strom_leitungen_31258, bundesland) # clip using Salzburg state polygon

# download data on FFH sites ----
ffh_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Europaschutzgebiete_FFH_RL/Europaschutzgebiete_FFH_RL.shp")

# download data on other Natura 2000 sites ----
natura2000_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Europaschutzgebiete_Schutzzonen/Europaschutzgebiete_Schutzzonen.shp")

# download data on SPA sites ----
spa_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Europaschutzgebiete_VS_RL/Europaschutzgebiete_VS_RL.shp")

# download data on RAMSAR sites ----
ramsar_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Gebiete_gem_Ramsar_Konvention/Gebiete_gem_Ramsar_Konvention.shp")

# download data on protected landscape features ----
geschuezte_lt_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Geschuetzte_Landschaftsteile/Geschuetzte_Landschaftsteile.shp")

# download data on protected natural features ----
geschuezte_ng_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Geschuetzte_Naturgebilde/Geschuetzte_Naturgebilde.shp")

# download data on landscape protection areas ----
lsg_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Landschaftsschutzgebiete/Landschaftsschutzgebiete.shp")

# download data on National Park sites ----
np_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/NPHT_Land_Salzburg/NPHT_Land_Salzburg.shp")

# download data on natural monuments ----
nd_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Naturdenkmaeler/Naturdenkmaeler.shp")

# download data on nature conservation areas ----
nsg_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Naturschutzgebiete/Naturschutzgebiete.shp")

# download data on plant protection areas ----
psg_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Pflanzenschutzgebiete/Pflanzenschutzgebiete.shp")

# download data on wildlife reserves ----
wsg_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Wild_Europaschutzgebiete_VS_RL/Wild_Europaschutzgebiete_VS_RL.shp")

# download data on municipality populations ----
pop_raw <- read_csv("https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Bev_2023_nach_Katastralgemeinden.csv",
                    skip = 1) # skip first row so that columns names are the headers


#### ------------------------ Further Data Cleaning ----------------------- ####
# clean the column names of all dataframes and spatial data frames ----
# Get a list of all data frames in the global environment
df_list <- Filter(is.data.frame, mget(ls()))

# Loop through each data frame, apply clean_names(), and rename the data frame
for (df_name in names(df_list)) {
  # Apply clean_names() to the data frame
  cleaned_df <- clean_names(get(df_name))
  
  # Change the name by appending "_clean" and removing "_raw" if present
  new_name <- gsub("_raw", "", paste0(df_name, "_clean"))
  
  # Assign the cleaned data frame with the new name to the global environment
  assign(new_name, clean_names(get(df_name)))
  
  rm(cleaned_df)
}

# merge together population data with geometry data of muncipality ----

# aggregate Kastralgemeinde population to the Gemeinde (municipal) level 
gemeinden <- pop_clean %>% 
  filter(bundesland == "Salzburg") %>%
  select(-c(4,5)) %>%
  rename(c(gkz = 2, bevölkerung = 4)) %>%
  group_by(gemeindename, gkz) %>%
  summarise(bevölkerung = sum(bevölkerung)) %>%
  # merge with gem_grenz_raw on basis of muncipality name & code
  right_join(gem_grenz_clean, by = c("gkz" = "gemnr", "gemeindename" = "name")) %>%
  st_as_sf() # reasign as sf object

# merge datasets on protected areas together ----

# merge cleaned datasets using bind_rows 
pa_all <- bind_rows(ffh_clean, geschuezte_lt_clean, geschuezte_ng_clean, 
                    lsg_clean, natura2000_clean, nd_clean, np_clean, nsg_clean, 
                    psg_clean, ramsar_clean, spa_clean, wsg_clean)


# extract water, roads, train tracks, solar parks, populated areas, ---- 
# enterprise areas, and residential areas from fl_wid_clean

# check validity of all geometries and correct if not
if ("geometry" %in% names(fl_wid_clean)) {
  
  # Check validity of geometries
  invalid_geoms <- st_is_valid(fl_wid_clean$geometry)
  
  # If there are invalid geometries, correct them using st_make_valid
  if (any(!invalid_geoms)) {
    # Identify and correct invalid geometries
    fl_wid_clean$geometry[!invalid_geoms] <- st_make_valid(fl_wid_clean$geometry[!invalid_geoms])
    
  }
  
}

# extra data for water, roads, train tracks, solar parks, populated areas, 
# enterprise areas and solar farms and add a key for these categories

fl_wid_bewohnt <- fl_wid_clean %>%
  filter(typname == "BARW" | typname == "BAEW" | typname == "BAFW" | 
           typname == "BAKG" | typname == "BALK" | typname == "BADG" | 
           typname == "BABE" | typname == "BAGG" | typname == "BAIG" | 
           typname == "BAZG" | typname == "BAHV" | typname == "BAHC" | 
           typname == "BAHF" | typname == "BAHB" | typname == "BAHE" | 
           typname == "BABG") %>%
  mutate(kategorie = "bewohnt")

fl_wid_wohngebiete <- fl_wid_clean %>%
  filter(typname == "BARW" | typname == "BAEW" | typname == "BAFW" | 
           typname == "BAKG" | typname == "BALK" | typname == "BADG" | 
           typname == "BAZG" | typname == "BABG") %>%
  mutate(kategorie = "wohngebiet")

fl_wid_betriebe <- fl_wid_clean %>%
  filter(typname == "BADG" | typname == "BABE" | typname == "BAGG" | 
           typname == "BAIG" | typname == "BAHV" | typname == "BAHC" | 
           typname == "BAHF" | typname == "BAHB" | typname == "BAHE" | 
           typname == "BABG" | typname == "GLMG") %>%
  mutate(kategorie = "betrieb")

fl_wid_gewaesser <- fl_wid_clean %>%
  filter(typname == "GLGG") %>%
  mutate(kategorie = "gewaesser")

fl_wid_bahn <- fl_wid_clean %>%
  filter(typname == "VEEB") %>%
  mutate(kategorie = "bahn")

fl_wid_str <- fl_wid_clean %>%
  filter(typname == "VEGM" | typname == "VEBL" | typname == "VESO") %>%
  mutate(kategorie = "strasse")

fl_wid_solar <- fl_wid_clean %>%
  filter(typname == "GLSA") %>%
  mutate(kategorie = "solar")

# combine these into a new spatial dataframe with a key for 
fl_wid_kat_alle <- bind_rows(list(fl_wid_bahn, fl_wid_betriebe, fl_wid_bewohnt, 
                                  fl_wid_gewaesser, fl_wid_str, 
                                  fl_wid_wohngebiete, fl_wid_solar))

#### -------------------------------- Roads ------------------------------- ####
# create buffer around roads
str_buffer <- fl_wid_kat_alle %>%
  filter(kategorie == "strasse") %>% # choose roads from df
  select(geometry) %>% # select only the geometry column for spatial operations
  st_union() %>% # combine all road geometries into one polygon geometry
  st_buffer(500)%>% # 500 m buffer around geometries
  st_intersection(bundesland_clean) # clip to size of state

# write str_buffer to a shapefile
st_write(str_buffer, "daten_zwischenablage/str_buffer.shp", append = F)

# read in str_buffer
str_buffer <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/str_buffer.shp")

#### --------------------------- Populated Areas -------------------------- ####
# create buffer around populated areas
# combine all populated area geometries into one polygon geometry
bewohnt_buffer <- fl_wid_kat_alle %>%
  filter(kategorie == "bewohnt") %>% # choose populated areas from df
  select(geometry) %>% # select only the geometry column for spatial operations
  st_union() %>% # combine all populated area geometries into one polygon geometry
  st_buffer(2500)%>% # 2500 m buffer around geometries
  st_intersection(bundesland_clean) # clip to size of state

# write bewohnt_buffer to a shapefile
st_write(bewohnt_buffer, "daten_zwischenablage/bewohnt_buffer.shp", append = F)

# read in bewohnt_buffer
bewohnt_buffer <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/bewohnt_buffer.shp")

#### -------------------------- Residential Areas ------------------------- ####
# create buffer around residential areas
# combine all residential area geometries into one polygon geometry
wohngebiete_buffer <- fl_wid_kat_alle %>%
  filter(kategorie == "wohngebiet") %>% # choose residential areas from df
  select(geometry) %>% # select only the geometry column for spatial operations
  st_union() %>% # combine all residential area geometries into one polygon geometry
  st_buffer(500)%>% # 500 m buffer around geometries
  st_intersection(bundesland_clean) # clip to size of state

# write wohngebiete_buffer to a shapefile
st_write(wohngebiete_buffer, "daten_zwischenablage/wohngebiete_buffer.shp", 
         append = F)

# read in wohngebiete_buffer
wohngebiete_buffer <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/wohngebiete_buffer.shp")

#### --------------------- Enterprise/Indsutrial Areas -------------------- ####
# create buffer around enterprise/industrial areas
# combine all enterprise/industrial area geometries into one polygon geometry
betriebe_buffer <- fl_wid_kat_alle %>%
  filter(kategorie == "betrieb") %>% # choose residential areas from df
  select(geometry) %>% # select only the geometry column for spatial operations
  st_union() %>% # combine all enterprise/industral area geometries into one polygon geometry
  st_buffer(3500)%>% # 3500 m buffer around geometries
  st_intersection(bundesland_clean) # clip to size of state

# write betriebe_buffer to a shapefile
st_write(betriebe_buffer, "daten_zwischenablage/betriebe_buffer.shp", 
         append = F)

# read in betriebe_buffer
betriebe_buffer <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/betriebe_buffer.shp")

#### --------------------------- Protected Areas -------------------------- #### 
# combine all polygons together on Protected Area Data
## use old dataframe for interactive leaflet map

# check validity of all geometries and correct if not
if ("geometry" %in% names(pa_all)) {
  
  # Check validity of geometries
  invalid_geoms <- st_is_valid(pa_all$geometry)
  
  # If there are invalid geometries, correct them using st_make_valid
  if (any(!invalid_geoms)) {
    # Identify and correct invalid geometries
    pa_all$geometry[!invalid_geoms] <- st_make_valid(pa_all$geometry[!invalid_geoms])
    
  }
  
  # Use st_union to join all pa geometries into one multipolygon
  pa_comb <- st_union(pa_all$geometry)
  
} 

#### ---------- Combine vector buffers and geometric conditions ----------- ####
# combine buffers outlining appropriate places - betriebe, bewohnt, str 
polygon_flaeche_geeignet_rein <- bind_rows(list(betriebe_buffer, bewohnt_buffer, 
                                                str_buffer)) %>%
  st_union()

# combine geometry of existing buildings, streets, protected areas and the 
# residential buffer
polygon_flaeche_ungeeignet_rein <- fl_wid_kat_alle %>%
  filter(kategorie == "wohngebiet" | kategorie == "bewohnt" | 
           kategorie == "betrieb" | kategorie == "gewaeser" | 
           kategorie == "bahn" | kategorie == "strasse") %>%
  select(geometry) %>%
  st_union() %>%
  st_union(wohngebiete_buffer) %>%
  st_union(pa_comb)

# clip the polygon of suitable areas to polygon_flaeche_ungeeignet_rein
polygon_flaeche_geeignet <- st_difference(polygon_flaeche_geeignet_rein, 
                                          polygon_flaeche_ungeeignet_rein)

st_write(polygon_flaeche_geeignet, "daten_zwischenablage/polygon_flaeche_geeignet.shp")

polygon_flaeche_geeignet <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/polygon_flaeche_geeignet.shp")

#### ----------------------------- Land Cover ----------------------------- ####
# 10 - Tree Cover
# 20 - Shrublands # suitable
# 30 - Grassland # suitable
# 40 - Cropland # suitable (try with and without)
# 50 - Built-up
# 60 - Bare/sparse vegetation # suitable
# 70 - Snow and ice
# 80 - Permanent water bodies
# 90 - Herbaceous wetland
# 100 - Moss and lichen

# load in landcover data for Salzburg
lc_sbg <- rast("daten_zwischenablage/lc_sbg.tif")

# mask to polygon_flaeche_geeignet 
lc_geeignet_fl <- mask(lc_sbg, polygon_flaeche_geeignet)

# set areas with suitable landcover to 1 and rest to na
lc_geeignet <- subst(lc_geeignet_fl, c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     c(NA, 1, 1, 1, NA, 1, NA, NA, NA, NA), others=NA)

writeRaster(lc_geeignet, "daten_zwischenablage/lc_geeignet.tif", overwrite = T)

lc_geeignet <- rast("daten_zwischenablage/lc_geeignet.tif")

#### ------------------------------- Height ------------------------------- ####

dgm <- rast("daten_zwischenablage/dgm.tif") 
dgm_geeignet_fl <- mask(dgm, polygon_flaeche_geeignet)
dgm_geeignet <- terra::clamp(dgm_geeignet_fl, upper=1500, value=FALSE)

#### ----------------------------- Topography ----------------------------- ####

# load in dgm
dgm <- rast("daten_zwischenablage/dgm.tif")

dgm_geeignet_fl <- mask(dgm, polygon_flaeche_geeignet)

dgm_geeignet <- terra::clamp(dgm_geeignet_fl, upper=1500, value=FALSE)

writeRaster(dgm_geeignet, "daten_zwischenablage/dgm_geeignet.tif")

# calculate aspect from the dsm
terra::terrain(dgm, v="aspect", neighbors=8, unit="degrees", filename="daten_zwischenablage/hangrichtung.tif", overwrite = T)

# calculate slope from dsm
terra::terrain(dgm, v="slope", neighbors=8, unit="degrees", filename="daten_zwischenablage/hangneigung.tif", overwrite = T)

hangausrichtung <- rast("daten_zwischenablage/hangausrichtung.tif")
hangneigung <- rast("daten_zwischenablage/hangneigung.tif")

# load in slope and aspect rasters

# hangausrichtung <- rast("offline_files/hangausrichtung5m/hangausrichtung5m.asc")
# hangneigung <- rast("offline_files/hangneigung5m/hangneigung5m.asc")

# no crs present - assign CRS based on metadata 
# crs(hangausrichtung) <- "EPSG: 31258" # (https://service.salzburg.gv.at/ogd/client/showDetail/fbeed257-28bf-44b1-9ab4-eeb80863b79c)
# crs(hangneigung) <- "EPSG: 31258" # https://service.salzburg.gv.at/ogd/client/showDetail/57c38869-3226-4023-8030-34b5f54e2f0b

# set areas with suitable gradient to 1 and rest to na
hangneigung_geeignet_fl <- terra::mask(hangneigung, polygon_flaeche_geeignet)

hangneigung_zahl <- c(0, 15, 1)

hangneigung_matr <- matrix(hangneigung_zahl, ncol=3, byrow=TRUE)

hangneigung_geeignet_alle <- classify(hangneigung_geeignet_fl, hangneigung_matr, others=NA)

# 3.5 degrees as threshold 
hangneigung_zahl_3_5 <- c(0, 3.5, 1)

hangneigung_matr_3_5 <- matrix(hangneigung_zahl_3_5, ncol=3, byrow=TRUE)

hangneigung_geeignet_unter_3_5 <- classify(hangneigung_geeignet_fl, hangneigung_matr_3_5, others=NA)

# set areas with suitable aspect to 1 and rest to na
hangausrichtung_geeignet_fl <- terra::mask(hangausrichtung, polygon_flaeche_geeignet)

hangausrichtung_zahl <- c(110, 200, 1)

hangausrichtung_matr <- matrix(hangausrichtung_zahl, ncol=3, byrow=TRUE)

hangausrichtung_geeignet <- classify(hangausrichtung_geeignet_fl, hangausrichtung_matr, others=NA)

# clip hangneigung_geeignet_alle with dgm
hn_dgM_geeignet <- terra::mask(hangneigung_geeignet_alle, dgm)

# clip with hangausrichtung
hn_dgm_ha_geeignet <- terra::mask(hn_dgM_geeignet, hangausrichtung_geeignet)

writeRaster(hn_dgm_ha_geeignet, "daten_zwischenablage/hn_dgm_ha_geeignet.tif")

# add in those areas under 3.5 degrees of gradient
topographie_geeignet <- terra::merge(hn_dgm_ha_geeignet, hangneigung_geeignet_unter_3_5)

writeRaster(topographie_geeignet, "daten_zwischenablage/topographie_geeignet.tif")
topographie_geeignet <- rast("daten_zwischenablage/topographie_geeignet.tif")

#### --------------------------- Solar Radiation -------------------------- ####
# resample solar_raw to a lower resolution based on dgm (otherwise R does not compute)
# also to allow for more contiguous areas - with 1x1 resolution - there would likely be very fragemetned areas
solar_resample <- resample(solar_raw, dgm)

# load in saved resampled raster
solar_resample <- rast("daten_zwischenablage/solar_resample.tif")

# set values of areas which do satisfy the 1100 kw/yr requirement to NA
solar_geeignet <- terra::clamp(solar_resample, lower=1100, value=FALSE)

# write 
writeRaster(solar_geeignet, "daten_zwischenablage/solar_geeignet.tif", overwrite = T)

#### ------------ Combine solar_geeignet & topographie_geeignet ----------- ####
# mask suitable topographical areas with areas with sufficient solar radiation
# done as these rasters are on the smae grid and at the same resolution
top_solar_geeignet <- terra::mask(topographie_geeignet, solar_geeignet)

# write out raster
writeRaster(top_solar_geeignet,"daten_zwischenablage/top_solar_geeignet.tif")

#### --------------------------- Convert to sf ---------------------------- ####

# Topography-Solar combined Raster
top_solar_geeignet_vec <- as.polygons(top_solar_geeignet)
top_solar_geeignet_vec <- st_as_sf(top_solar_geeignet_vec) %>%
  select(geometry)

# lc raster
lc_geeignet_vec <- as.polygons(lc_geeignet)
lc_geeignet_vec <- st_as_sf(lc_geeignet_vec) %>%
  select(geometry)

# find intersection between solar, topography and lc areas
flaeche_geeignet <- st_intersection(top_solar_geeignet_vec, lc_geeignet_vec)

# write out shp
st_write(flaeche_geeignet, "daten_zwischenablage/flaeche_geeignet.shp")

# read in flaeche_geeignet
flaeche_geeignet <- st_read("daten_zwischenablage/flaeche_geeignet.shp")

#### ------------------------- Cluster Analysis -------------------------- ####
# calculate the amount of solar panel area in each municipality
# Function to calculate area for each polygon
#run the intersect function, converting the output to a tibble in the process
gemeinden_solar <- as_tibble(st_intersection(flaeche_geeignet, gemeinden))

#add in an area count column to the tibble (area of each arable poly in intersect layer)
gemeinden_solar$flaeche_geeignet_in_gemeinde <- st_area(gemeinden_solar$geometry)

# write out shp
st_write(gemeinden_solar, "daten_zwischenablage/gemeinden_solar.shp")

#### --------------------------- Map and Graphs -------------------------- ####

# mutate area of grassland of each Gemeinde to Gemeinde

# Maps: map on PAs, map on built-up areas, map on land-use, topographical map
# map of municipalities

# inset map of area

# see which criteria make sense and download accordingly
# save these datasets offline as they are large (or write the shp at least onto github if fits )

# check overlapping IDs of OSM data

# wo darf man in Salzburg keine Solaranlagen bauen

# Liftanlagen nicht berücksichtigt - Nutzungskonflikt
# mit Schifahren

# Strassen und Gebäuden mit Overpass API - schauen ob
# die Daten von den GLM geeignet sind oder nicht

# DLM_2000_BAUTEN_20230912 - stromleitungen

# ob es politisch durchsetzbar ist, koennte eine folgende Stuide sein
# decision support system - wo werden die größte Anzahl von Einwohnern betroffen
