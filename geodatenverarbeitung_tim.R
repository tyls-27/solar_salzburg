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

# download data on SPA sites (second version - check if same as first) ----
spa_2_raw <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Wild_Europaschutzgebiete_VS_RL/Wild_Europaschutzgebiete_VS_RL.shp")

# download data on municipality populations ----
pop_raw <- read_csv("https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/Bev_2023_nach_Katastralgemeinden.csv",
                    skip = 1) # skip first row so that columns names are the headers

# download data from OSM on roads ----
# define bounding box based on bb from bundesland sf object
osm_bbox <- st_bbox(bundesland)
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
# sbg_str_comb <- bind_rows(sbg_str_main, sbg_str_link)

# write out shp
# st_write(sbg_str_comb, "daten_zwischenablage/sbg_strassen.shp", append = F)

# load in shp on road data in Salzburg
sbg_str_comb <- st_read("/vsicurl/https://raw.githubusercontent.com/tyls-27/solar_salzburg/master/daten_zwischenablage/sbg_strassen.shp")

# clip to size of Salburg province
sbg_str_comb <- st_intersection(sbg_str_comb, bundesland) # clip using Salzburg state polygon

# pass our bounding box coordinates into the OverPassQuery (opq) function
sbg_track_osm <- opq(bbox = osm_bbox) %>%
  # pipe this into the add_osm_feature data query function to extract our tracks
  add_osm_feature(key = "highway", value = "track") %>%
  # pipe this into our osmdata_sf object
  osmdata_sf()

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
# merge pop_raw and gem_grenz_raw ----

# aggregate Kastralgemeinde population to the Gemeinde (municipal) level 
pop_clean <- pop_raw %>%
  filter(Bundesland == "Salzburg") %>%
  select(-c(4,5)) %>%
  rename(c(GKZ = 2, Bevölkerung = 4)) %>%
  group_by(Gemeindename, GKZ) %>%
  summarise(Bevölkerung = sum(Bevölkerung))

# merge pop_clean and gem_grenz_raw on basis of muncipality name & code
gemeinden <- gem_grenz_raw %>%
  left_join(pop_clean, by = c("GEMNR" = "GKZ", "NAME" = "Gemeindename"))

# check osm ids ----
# double check all of the other datasets (no need to leave code) ----
#### Topography ####


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

