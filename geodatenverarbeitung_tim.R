# Solarenergie- und -anlagenpotenzial Salzburgs - eine Analyse und tentative Machbarkeitsstudie
# Date: January 2024
# Author: Timothy Sung

library(tmap)
library(raster)
library(terra)
library(sp)
library(sf)
library(tidyverse)
library(osmdata)
library(janitor)

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

# no crs present - assign CRS based on metadata (https://service.salzburg.gv.at/ogd/client/showDetail/d585e816-1a36-4c76-b2dc-6db487d22ca3)
crs(dgm) <- "EPSG: 31258"

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

# download data from OSM on roads ----
# define bounding box based on bb from bundesland sf object
osm_bbox <- st_bbox(st_transform(bundesland, "EPSG: 4326"))
# pass our bounding box coordinates into the OverPassQuery (opq) function
#sbg_strassen_osm <- opq(bbox = osm_bbox) %>%
  # pipe this into the add_osm_feature data query function to extract our roads
  #add_osm_features(features= list (
   # "highway" = "motorway",
   # "highway" = "trunk",
   # "highway" = "primary",
   # "highway" = "tertiary",
   #"highway" = "unclassified"
  # )) %>%
  # pipe this into our osmdata_sf object
  # osmdata_sf()

# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_str_link_osm <- opq(bbox = osm_bbox) %>%
  # pipe this into the add_osm_feature data query function to extract our road links
#  add_osm_features(features= list (
 #   "highway" = "motorway_link",
 #   "highway" = "trunk_link",
 #   "highway" = "primary_link",
 #   "highway" = "tertiary_link"
 # )) %>%
  # pipe this into our osmdata_sf object
 # osmdata_sf()

# prepare both road datasets for merging
# main roads
# sbg_str_main <- sbg_strassen_osm$osm_lines %>%
#  select(c(1, 2, 234, 669)) %>% # select the osm_id, name, highway value and geometry columns
#  st_transform("EPSG: 31258") # reproject to working CRS

# link roads
# sbg_str_link <- sbg_str_link_osm$osm_lines %>%
#  select(c(1, 2, 147)) %>% # select the osm_id, name, and geometry columns
#  mutate(highway = 'link', .after = name) %>% # add highway value column
#  st_transform("EPSG: 31258") # reproject to working CRS

# merge the two spatial dfs into one spatial df
# sbg_str_all <- bind_rows(sbg_str_main, sbg_str_link)

# write out shp
# st_write(sbg_str_all, "daten_zwischenablage/sbg_strassen.shp", append = F)

# load in shp on road data in Salzburg
sbg_str_all <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/sbg_strassen.shp")

# clip to size of Salburg province
sbg_str_all <- st_intersection(sbg_str_comb, bundesland) # clip using Salzburg state polygon

# download data from OSM on power lines (work out if osm or sbg data is better)----
# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_sl_osm <- opq(bbox = osm_bbox) %>%
  # pipe this into the add_osm_feature data query function to extract our power lines
#  add_osm_feature(key = "power", value = "line") %>%
  # pipe this into our osmdata_sf object
#  osmdata_sf()

# extract power line lines  from the downloaded OSM dataset
# sbg_sl <- sbg_sl_osm$osm_lines %>%
#   dplyr::select(c(1, 2, 25, 35)) %>% # select osm_id, name, power value and geometry columns
#   st_transform("EPSG: 31258") # reproject to working CRS

# write out shapefile
# st_write(sbg_sl, "daten_zwischenablage/sbg_sl.shp", append = F)

# load in shp on road data in Salzburg
sbg_sl <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/sbg_sl.shp")

# clip to size of Salburg province
sbg_sl <- st_intersection(sbg_sl, bundesland) # clip using Salzburg state polygon

# download data from OSM on developed and residential areas ----
# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_wohngebiet_osm <- opq(bbox = osm_bbox) %>%
  # pipe this into the add_osm_feature data query function to extract our residential areas
#  add_osm_feature(key = "landuse", value = "residential") %>%
  # pipe this into our osmdata_sf object
#  osmdata_sf()

# extract residential polygons from the downloaded OSM dataset
# sbg_wohngebiet <- sbg_wohngebiet_osm$osm_polygons %>%
#  filter(landuse == 'residential') %>% # only use entries marked residential
  # dplyr::select(c(1, 2, 47, 136)) %>% # select osm_id, name, landuse value and geometry columns
  # st_transform("EPSG: 31258") # reproject to working CRS

# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_hof_osm <- opq(bbox = osm_bbox) %>%
  # pipe this into the add_osm_feature data query function to extract our farmyards (contains homes)
#   add_osm_feature(key = "landuse", value = "farmyard") %>%
  # pipe this into our osmdata_sf object
#   osmdata_sf()

# extract farmyard polygons from the downloaded OSM dataset
# sbg_hof <- sbg_hof_osm$osm_polygons %>%
#   filter(landuse == 'farmyard') %>% # only use entries marked farmyard
#   dplyr::select(c(1, 2, 37, 68)) %>% # select osm_id, name, landuse value and geometry columns
#   st_transform("EPSG: 31258") # reproject to working CRS

# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_gewerbe_osm <- opq(bbox = osm_bbox) %>%
   # pipe this into the add_osm_feature data query function to extract our commercial areas
#   add_osm_feature(key = "landuse", value = "commercial") %>%
   # pipe this into our osmdata_sf object
#   osmdata_sf()
 
# extract commercial area polygons from the downloaded OSM dataset
# sbg_gewerbe <- sbg_gewerbe_osm$osm_polygons %>% 
#   filter(landuse == 'commericial') %>% # only use entries marked commercial
#   dplyr::select(c(1, 2, 41, 80)) %>% # select osm_id, name, landuse value and geometry columns
#   st_transform("EPSG: 31258") # reproject to working CRS
 
# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_industrie_osm <- opq(bbox = osm_bbox) %>%
   # pipe this into the add_osm_feature data query function to extract our industrial areas
#   add_osm_feature(key = "landuse", value = "industrial") %>%
   # pipe this into our osmdata_sf object
#   osmdata_sf()
 
# extract industrial area polygons from the downloaded OSM dataset
# sbg_industrie <- sbg_industrie_osm$osm_polygons %>% 
#   filter(landuse == 'industrial') %>% # only use entries marked geometry
#   dplyr::select(c(1, 2, 56, 134)) %>% # select osm_id, name, landuse value and geometry columns
#   st_transform("EPSG: 31258") # reproject to working CRS
 
# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_handel_osm <- opq(bbox = osm_bbox) %>%
   # pipe this into the add_osm_feature data query function to extract our retail areas
#   add_osm_feature(key = "landuse", value = "retail") %>%
   # pipe this into our osmdata_sf object
#   osmdata_sf()
 
# extract retail area polygons from the downloaded OSM dataset
# sbg_handel <- sbg_handel_osm$osm_polygons %>%
#   filter(landuse == 'retail') %>% # only use entries marked retail
#   dplyr::select(c(1, 2, 19, 42)) %>% # select osm_id, name, landuse value and geometry columns
#   st_transform("EPSG: 31258") # reproject to working CRS
 
# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_bildung_osm <- opq(bbox = osm_bbox) %>%
   # pipe this into the add_osm_feature data query function to extract our areas of education
#   add_osm_feature(key = "landuse", value = "education") %>%
   # pipe this into our osmdata_sf object
#   osmdata_sf()
 
# extract areas of education polygons from the downloaded OSM dataset
# sbg_bildung <- sbg_bildung_osm$osm_polygons %>%
#   dplyr::select(c(1, 2, 9, 18)) %>% # select osm_id, name, landuse value and geometry columns
#   st_transform("EPSG: 31258") # reproject to working CRS
 
# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_inst_osm <- opq(bbox = osm_bbox) %>%
   # pipe this into the add_osm_feature data query function to extract our areas of insitutional land-use
#   add_osm_feature(key = "landuse", value = "institutional") %>%
   # pipe this into our osmdata_sf object
#   osmdata_sf()
 
# extract areas of insitutional land-use polygons from the downloaded OSM dataset
# sbg_inst <- sbg_inst_osm$osm_polygons %>%
#   mutate(name = NA, .after = osm_id) %>% # add a placeholder name column
#   st_transform("EPSG: 31258") # reproject to working CRS
 
# combine spatial dfs of all developed land-use areas
# sbg_alle_geb <- bind_rows(list(sbg_bildung, sbg_gewerbe, sbg_handel, sbg_hof,
#                                sbg_industrie, sbg_inst, sbg_wohngebiet))
 
# combine spatial dfs of all residential/populated areas
# sbg_wohngebiete <- bind_rows(list(sbg_hof, sbg_wohngebiet))
 
# combine spatial dfs of areas with enterprises
# sbg_betriebe <- bind_rows(list(sbg_gewerbe, sbg_handel, sbg_industrie))
 
# write out shp for each spatial sf on developed and residential areas
# st_write(sbg_alle_geb, "daten_zwischenablage/sbg_alle_geb.shp", append = F)
# st_write(sbg_wohngebiete, "daten_zwischenablage/sbg_wohngebiete.shp", append = F)
# st_write(sbg_betriebe, "daten_zwischenablage/sbg_betriebe.shp", append = F)

# load in shps on developed and residential areas
sbg_alle_geb <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/sbg_alle_geb.shp")
sbg_wohngebiete <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/sbg_wohngebiete.shp")
sbg_betriebe <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/sbg_betriebe.shp")

# clip to size of Salburg province
sbg_alle_geb <- st_intersection(sbg_alle_geb, bundesland) # clip using Salzburg state polygon
sbg_wohngebiete <- st_intersection(sbg_wohngebiete, bundesland) # clip using Salzburg state polygon
sbg_betriebe <- st_intersection(sbg_betriebe, bundesland) # clip using Salzburg state polygon


 
# download data from OSM on train lines ----
# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_zradbahn_osm <- opq(bbox = osm_bbox) %>%
   # pipe this into the add_osm_feature data query function to extract our funicular railways
#   add_osm_feature(key = "railway", value = "funicular") %>%
   # pipe this into our osmdata_sf object
#   osmdata_sf()
 
# extract funicular lines from the downloaded OSM dataset
# sbg_zradbahn <- sbg_zradbahn_osm$osm_lines %>%
#   dplyr::select(c(1, 2, 21, 34)) %>% # select osm_id, name, railway value, and geometry columns
#   st_transform("EPSG: 31258") # reproject to working CRS
 
# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_lokalbahn_osm <- opq(bbox = osm_bbox) %>%
   # pipe this into the add_osm_feature data query function to extract our light rail lines
#   add_osm_feature(key = "railway", value = "light_rail") %>%
   # pipe this into our osmdata_sf object
#   osmdata_sf()
 
# extract light rail lines from the downloaded OSM dataset
# sbg_lokalbahn <- sbg_lokalbahn_osm$osm_lines %>%
#   dplyr::select(c(1, 2, 16, 27)) %>% # select osm_id, name, railway value, and geometry columns
#   st_transform("EPSG: 31258") # reproject to working CRS
 
# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_schmalspur_osm <- opq(bbox = osm_bbox) %>%
   # pipe this into the add_osm_feature data query function to extract our narrow gauge railway lines
#   add_osm_feature(key = "railway", value = "narrow_gauge") %>%
   # pipe this into our osmdata_sf object
#   osmdata_sf()
 
# extract narrow gauge lines from the downloaded OSM dataset
# sbg_schmalspur <- sbg_schmalspur_osm$osm_lines %>%
#   dplyr::select(c(1, 2, 31, 49)) %>% # select osm_id, name, railway value, and geometry columns
#   st_transform("EPSG: 31258") # reproject to working CRS
 
# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_tram_osm <- opq(bbox = osm_bbox) %>%
   # pipe this into the add_osm_feature data query function to extract our tram lines
#   add_osm_feature(key = "railway", value = "tram") %>%
   # pipe this into our osmdata_sf object
#   osmdata_sf()
 
# extract tram lines from the downloaded OSM dataset
# sbg_tram <- sbg_tram_osm$osm_lines %>%
#   dplyr::select(c(1, 2, 22, 14)) %>% # select osm_id, name, railway value, and geometry columns
#   st_transform("EPSG: 31258") # reproject to working CRS
 
# pass our bounding box coordinates into the OverPassQuery (opq) function
# sbg_hauptstrecken_osm <- opq(bbox = osm_bbox) %>%
   # pipe this into the add_osm_feature data query function to extract our main railway lines
#   add_osm_feature(key = "railway", value = "rail") %>%
   # pipe this into our osmdata_sf object
#   osmdata_sf()
 
# extract rail lines from the downloaded OSM dataset
# sbg_hauptstrecken <- sbg_hauptstrecken_osm$osm_lines %>%
#   dplyr::select(c(1, 2, 45, 77)) %>% # select osm_id, name, railway value, and geometry columns
#   st_transform("EPSG: 31258") # reproject to working CRS
 
# combine all spatial dfs on train lines into one object
# sbg_bahn <- bind_rows(list(sbg_hauptstrecken, sbg_lokalbahn, sbg_schmalspur,
#                            sbg_tram, sbg_zradbahn))
 
# write out shp
# st_write(sbg_bahn, "daten_zwischenablage/sbg_bahn.shp", append = F)

# load in shps on developed and residential areas
sbg_bahn <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/sbg_bahn.shp")

# clip to size of Salburg province
sbg_bahn <- st_intersection(sbg_bahn, bundesland) # clip using Salzburg state polygon

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

# check osm ids ----

# using osm_id:
# check if there are any duplicates in data on all developed land-use areas
any(duplicated(sbg_alle_geb_clean$osm_id)) # false

# check if there are any duplicates in data on railway lines
any(duplicated(sbg_bahn_clean$osm_id)) # false

# check if there are any duplicates in data on enterprise lines
any(duplicated(sbg_betriebe_clean$osm_id)) # false

# check if there are any duplicates in data on power lines
any(duplicated(sbg_sl_clean$osm_id)) # false

# check if there are any duplicates in data on roads
any(duplicated(sbg_str_comb_clean$osm_id)) # false

# check if there are any duplicates in data on residential areas
any(duplicated(sbg_wohngebiete_clean$osm_id)) # false

# double check all of the other datasets (no need to leave code) ----
# combine nature conservation areas into one polygon - check for overaps and then combine - fuse borders
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

# extra data for water, roads, train tracks, solar parks, populated areas, and 
# enterprise areas and add a key for these categories

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

#### ------------------------------- Roads -------------------------------- ####
# combine all road geometries into one line/multiline geometry
# sbg_str_comb <- st_union(sbg_str_all_clean$geometry)

# create a 500 m buffer around the line geometry of the road system
# str_buffer <- st_buffer(sbg_str_comb, 500) %>%
#   st_intersection(bundesland) (maybe with line for road? - or just combine with sbg_str_comb)


# write road_buffer to a shapefile
# st_write(road_buffer, "daten_zwischenablage/str_buffer.shp", append = F)

# read in road_buffer
str_buffer <- st_read("daten_zwischenablage/str_buffer.shp")
  
# include areas that touch the 500m buffer border

#### ----------------------------- Railways (see what criteria is best) ------------------------------- ####
# combine all rail lines geometries into one polygon
# check validity of all geometries and correct if not

# combine all road geometries into one line/multiline geometry
sbg_bahn_comb <- st_union(sbg_str_all_clean$geometry)

# create a 500 m buffer around the line geometry of the road system
sbg_bahn_buffer <- st_buffer(sbg_bahn_comb, 500) %>%
  st_intersection(bundesland) 


# write str_buffer to a shapefile
st_write(sbg_bahn_buffer, "daten_zwischenablage/bahn_buffer.shp", append = F)

# read in str_buffer
bahn_buffer <- st_read("daten_zwischenablage/bahn_buffer.shp")

#### ---------------------------- Developments ---------------------------- ####
## all developments
# combine all developments' geometries into one polygon
# check validity of all geometries and correct if not

# combine all developments' geometries into one line/multiline geometry
sbg_alle_geb_comb <- st_union(sbg_alle_geb_clean$geometry)

# create a 2500 m buffer around the polygon geometry of all developments in populated areas
sbg_alle_geb_buffer <- st_buffer(sbg_alle_geb_comb, 2500) %>%
  st_intersection(bundesland) 


# write sbg_alle_geb_buffer to a shapefile
st_write(sbg_alle_geb_buffer, "daten_zwischenablage/alle_geb_buffer.shp", append = F)

# read in road_buffer
alle_geb_buffer <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/alle_geb_buffer.shp")

## enterprises
# combine all enterprises' geometries into one polygon
# check validity of all geometries and correct if not

# combine all road geometries into one polygon geometry
sbg_betriebe_comb <- st_union(sbg_betriebe_clean$geometry)

# create a 3500 m buffer around the polygon geometry of all enterprise areas
sbg_betriebe_buffer <- st_buffer(sbg_betriebe_comb, 3500) %>%
  st_intersection(bundesland) 


# write sbg_all_geb_buffer to a shapefile
st_write(sbg_betriebe_buffer, "daten_zwischenablage/betriebe_buffer_buffer.shp", append = F)

# read in sbg_all_geb_buffer
betriebe_buffer <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/betriebe_buffer_buffer.shp")

## residential
# combine all residential geometries into one polygon
# check validity of all geometries and correct if not

# combine all road geometries into one polygon geometry
sbg_wohngebiete_comb <- st_union(sbg_wohngebiete_clean$geometry)

# create a 3500 m buffer around the polygon geometry of all enterprise areas
sbg_wohngebiete_buffer <- st_buffer(sbg_wohngebiete_comb, 500) %>%
  st_intersection(bundesland) 


# write sbg_all_geb_buffer to a shapefile
st_write(sbg_wohngebiete_buffer, "daten_zwischenablage/wohngebiete_buffer.shp", append = F)

# read in sbg_all_geb_buffer
wohngebiete_buffer <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/wohngebiete_buffer.shp")

#### TO DO: Railway clarify, electricity net clarify, populated areas clarify (where people live and work), and then work on raster operations / summarise the vector operations to create a mask for the raster areas
#### populated areas should be all but industry


#### --------------------------- Populated Areas -------------------------- ####
tm_shape(st_transform(bundesland, "EPSG: 4326")) + tm_polygons() + tm_shape(sbg_bewohnt_osm$osm_polygons) + tm_polygons()


#### ---------- Combine vector buffers and geometric conditions ----------- ####
# clip betriebe_buffer

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

# clip raster to polygon buffers 

# set all raster cells which are suitable to 1 and the others to na

#### Topography ####
# calculate aspect from the dsm
# terra::terrain(dgm, v="aspect", neighbors=8, unit="degrees", filename="daten_zwischenablage/hangrichtung.tif")

# calculate slope from dsm
# terra::terrain(dgm, v="slope", neighbors=8, unit="degrees", filename="daten_zwischenablage/hangneigung.tif")

hangausrichtung <- rast("daten_zwischenablage/hangausrichtung.tif")
hangneigung <- rast("daten_zwischenablage/hangneigung.tif")

# load in slope and aspect rasters

# hangausrichtung <- rast("offline_files/hangausrichtung5m/hangausrichtung5m.asc")
# hangneigung <- rast("offline_files/hangneigung5m/hangneigung5m.asc")

# no crs present - assign CRS based on metadata 
# crs(hangausrichtung) <- "EPSG: 31258" # (https://service.salzburg.gv.at/ogd/client/showDetail/fbeed257-28bf-44b1-9ab4-eeb80863b79c)
# crs(hangneigung) <- "EPSG: 31258" # https://service.salzburg.gv.at/ogd/client/showDetail/57c38869-3226-4023-8030-34b5f54e2f0b

# lower than 1500 m

#### --------------------------- Map and Graphs --------------------------- ####

# Maps: map on PAs, map on built-up areas, map on land-use, topographical map
# map of municipalities

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

