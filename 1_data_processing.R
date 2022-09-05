# Introduction ------------------------------------------------------------

# This script performs the initial cleaning of all data required for the land development model.

# Data will be pulled and cleaned as follows:

# Source 1: HARMONY 2022 (land use transportation interaction model)
#     data: study area boundary - tur_luti
#           study area zones - luti_zone
  
# Source 2: PPR 2021 (regional landscape plan) 
#     data: conservation area - ppr_ca
  
# Source 3: PTC2 2015 (regional strategic plan)
#     data: unesco heritage sites - ptc_unesco
#           urban green areas - ptc_urbg
#           forest cover - ptc_fc
  
# Source 4: PAI 2021 (po river authority flood plan)
#     data: floos zones - pai_fz
  
# Source 5: BDTRE 2022 (regional reference database)
#     data: road surface - bdtre_road
#           rail surface - bdtre_rail
#           water surface - bdtre_wtr
  
# Source 6: PRG to 2021 (land use plan for each municipality)
#     data: industrial land use - prg_ind
#           parks and gardens land use - prg_png
#           cemetary land use - prg_cem
#           educational land use - prg_edu
#           healthcare land use - prg_hlth

# Source 7: LIDAR 2011 (digital elevation model)
#     data: slope - dem_slp


# Setup Environment ------------------------------------------------------------

# load packages

# for input / outputs
library(here)

# for data manipulation
library(dplyr)
library(data.table)
library(lwgeom)
library(raster)
library(tidyverse)
library(magrittr)
library(units)
library(sf)
library(spatstat)
library(spatstat.geom)
library(stringr)
library(smoothr)
library(terra)
library(tibble)
library(tidyr)
library(tidyselect)

# for plotting
library(ggplot2)
library(ggsn)
library(ggspatial)
library(tmap)
library(cowplot)

# set optional preferences

# set working directory for our inputs/outputs and confirm (optional)
setwd()

# set options for simple features
options(stringsAsFactors = FALSE)

# some of our files are large, so we will raise the time out to 5 minutes
options(timeout=600)

# set a common crs to EPSG 32632 - WGS 84 / UTM zone 32N
# same crs as bdtre data which is most recent (2022) and forms the base level of the analysis
common_crs = 32632

# free memory (fm) not being used to lessen the load on our computer
fm <- gc()


# PART 1 - Import & Process LUTI data ------------------------------------------

# unzip LUTI results
unzip(zipfile="raw_data/zipped/TUR_results.zip", exdir="raw_data/unzipped")

# load LUTI results
zones_luti <- st_read(here::here("raw_data/unzipped/TUR_results/TUR_results.shp"),
                      stringsAsFactors = FALSE) %>%
  st_transform(., common_crs) %>%
  st_make_valid(.)

# have a peek
str(zones_luti)

# add area of each zone to a new column for later
# this will be used for density calculations later
zones_luti$area <- st_area(zones_luti)

# rename population change to percentage (pct)
zones_luti <- zones_luti %>%
  rename(PopCh19_30pct = PopCh19_30) %>%
  mutate(PopCH19_30rl = OiPred_T_1 - OiPred_Tot)

# create a new column with the real (rl) population change
# this will be used for density calculations later
zones_luti <- zones_luti %>%
  mutate(PopCH19_30rl = OiPred_T_1 - OiPred_Tot) %>%
  dplyr::rename(zones_luti, "PopCh19_30" = "PopCh19_30pct")

# confirm changes
head(zones_luti)

# Subset zones with the highest percentage change in population
lutitop3pct_loss <- zones_luti %>% slice_min(PopCh19_30pct, n = 3)
lutitop3pct_gain <- zones_luti %>% slice_max(PopCh19_30pct, n = 3)

# Subset zones with highest real change in population
lutitop3rl_loss <- zones_luti %>% slice_min(PopCH19_30rl, n = 3)
lutitop3rl_gain <- zones_luti %>% slice_max(PopCH19_30rl, n = 3)

# plot to confirm
# plot A
a_pct <- ggplot() +
  labs(title = "Top Three Zones in Turin Functional Urban Area by Projected Population Gain or Loss, 2019-2030",
       subtitle = "A. Percentage Change") +
  geom_sf(data = zones_luti, color = 'black', fill=NA, size = 0.05) +
  geom_sf(data = lutitop3pct_loss, color = '#e0644a', fill='#debdb6') +
  geom_sf(data = lutitop3pct_gain, color = '#4f5de0', fill='#b6b9de') +
  theme_void()

# plot B
b_rl <- ggplot() +
  labs(subtitle = "B. Real Change") +
  geom_sf(data = zones_luti, color = 'black', fill=NA, size = 0.05) +
  geom_sf(data = lutitop3rl_loss, color = '#e0644a', fill='#debdb6') +
  geom_sf(data = lutitop3rl_gain, color = '#4f5de0', fill='#b6b9de') +
  theme_void() +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl",height = unit(0.1, "cm"))

# test plot
test <- plot_grid(ncol = 2, align = "hv", a_pct, b_rl, label_size = 9) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))
test

# the plots seem to show a cluster of population gain just west of the city centre
# looking through the FUA zones online these zones are 668, 670, 1002, 1016
# we will filter for these zones for our local analysis
zones_loc <- zones_luti %>% 
  filter(NO == 668 | NO == 670 | NO == 1002 | NO == 1016)

# plot C
c_loc <- ggplot() +
  labs(subtitle = "C. Zones Selected for Local Analysis",
       caption = "SOURCE: HARMONY LUTI MODEL, 2022") +
  geom_sf(data = zones_luti, color = 'black', fill=NA, size = 0.05) +
  geom_sf(data = zones_loc, color = 'black', fill='lightgrey') +
  theme_void()

# replotwith Plot c
plot_lutitop3 <- plot_grid(ncol = 3,
                           a_pct, b_rl, c_loc,
                           align = "hv")
plot_lutitop3

# save plot
ggsave(filename = "plot_lutitop3.png",
       device = "png",
       path = here::here("figs/"),
       width = 9,
       height = 3,
       units = "in",
       dpi = 300)

# create local and regional study area boundaries for clipping
#fill any holes below a threshold
area_thresh <- units::set_units(1000, km^2)

# regional boundary
tur_luti <- zones_luti %>%
  st_union(.) %>%
  fill_holes(., threshold = area_thresh)

# local boundary
tur_loc <- zones_loc %>%
  st_union(.) %>%
  fill_holes(., threshold = area_thresh)

# check with plot
plot_study_areas <- ggplot() +
  labs(title = "Turin Functional Urban Area",
       subtitle = "Regional and Local Study Areas",
       caption = "SOURCE: HARMONY LUTI MODEL, 2022") +
  geom_sf(data = zones_luti, color = 'black', fill='#e1e2eb', size = 0.05) +
  geom_sf(data = tur_loc, color = 'black', fill='#b1b2cc') +
  geom_sf(data = tur_luti, color = 'black', fill=NA) +
  theme_void() +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl",height = unit(0.1, "cm"))
plot_study_areas

# save plot
ggsave(filename = "plot_study_areas.png",
       device = "png",
       path = here::here("figs/"),
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)

# export zones_luti to geopackage
st_write(obj = zones_luti,
         here::here("clean_data/zones_luti.gpkg"),
         delete_layer = TRUE)

# export zones_loc to geopackage
st_write(obj = zones_loc,
         here::here("clean_data/zones_loc.gpkg"),
         delete_layer = TRUE)

# export tur_luti to geopackage
st_write(obj = tur_luti,
         here::here("clean_data/tur_luti.gpkg"),
         delete_layer = TRUE)

# export tur_loc to geopackage
st_write(obj = tur_loc,
         here::here("clean_data/tur_loc.gpkg"),
         delete_layer = TRUE)

# remove unneeded variables
rm(list = c("a_pct", "b_rl", "c_loc", "test"))

# free memory
fm 


# PART 2 - Functions ------------------------------------------

# the function below is modified from Datacast's YouTube tutorial
# see https://www.youtube.com/watch?v=JPH6KE6kVCk

# function to download zip files from url
# arguments:
  # list_of_urls <- list of target url(s) for download - NOTE: create list with c(list)
  # dest_path <- location to download zip file(s) - NOTE: / at end creates new folder
download_zips <- function(list_of_urls){
  target_files <- basename(list_of_urls) %>% str_replace("(?<=.zip).*$","")
  download.file(list_of_urls, destfile = paste0(dest_path, target_files), method = "libcurl")
  zipped_files <- list.files(dest_path, pattern = ".zip$", full.names = TRUE)
  zipped_files
}

# NOTE:
  # I created a function to batch unzip files to a specified folder 
  # the function worked on a few zip files, but over half threw "zip file corrupted" errors
  # tried resolving, but resorted to manual unzip (for now)
  
# function to read, transform, make_valid, and clip shape files to tur_luti
# arguments:
  # shppath <- location of shape file to process
process_shp <- function(shppath){
  st_read(shppath) %>%
    st_transform(., common_crs) %>%
    st_make_valid(.) %>%
    st_intersection(., tur_luti)
}

# function to clip regional to local
# y <- local layer ex. ppr_ca_loc
# x <- regional layer ex. ppr_ca
clip_local <- function(x){
  y <- x %>%
    st_intersection(., tur_loc)
}

# remove unneeded variables
#rm(list = c(", "", "", ""))

# free memory
fm 


# PART 3 - Import & Process PPR data ----------------------------------------------------------

# use download_zips function to download conservation areas data
list_of_urls <- c("http://giscartografia.csi.it/Parchi/parchi_wgs84.zip")
dest_path = "raw_data/zipped/"
download_zips(list_of_urls)
  
# manual unzip - see note in PART 2 - Functions

# transform and clip to study area with function
Rppr_ca <- process_shp("raw_data/unzipped/ppr_ca/parchi_wgs84.shp")

# merge to one feature
Rppr_ca <- st_union(Rppr_ca)

# plot turin
ggplot() + 
  geom_sf(data = Rppr_ca, color = 'lightgreen', fill = 'lightgreen') +
  geom_sf(data = zones_luti, color = 'grey', fill=NA) +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  geom_sf(data = tur_luti, col='black', fill=NA) +
  theme_void()

# export to geopackage
st_write(obj = Rppr_ca,
         here::here("clean_data/Rppr_ca.gpkg"),
         delete_layer = TRUE)

# use clip_local function
Lppr_ca <- clip_local(Rppr_ca)

# plot local
ggplot() + 
  geom_sf(data = Lppr_ca, color='lightgreen', fill = 'lightgreen') +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  theme_void()

# export local to geopackage
st_write(obj = Lppr_ca,
         here::here("clean_data/Lppr_ca.gpkg"),
         delete_layer = TRUE)

# remove unneeded variables
#rm(list = c(", "", "", ""))

# free memory
fm  


# PART 4 - Import & Process PTC2 Data --------------------------------------------


# use download_zips function to download urban green areas data
list_of_urls <- c("http://www.cittametropolitana.torino.it/cms/risorse/territorio/dwd/pianificazione-territoriale/ptc2/shape_tavole/tav31/verde_urbano.zip")
dest_path <- "raw_data/zipped/"
download_zips(list_of_urls)

# manual unzip - see note in PART 2 - Functions

# transform and clip to study area with function
Rptc_urbg <- process_shp("raw_data/unzipped/ptc_urbg/verde_urbano.shp")

# merge to one feature
Rptc_urbg <- st_union(Rptc_urbg)

# plot to confirm
ggplot() + 
  geom_sf(data = Rptc_urbg, color='#3e6e12', fill='#3e6e12') +
  geom_sf(data = zones_luti, color = 'grey', fill=NA) +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  geom_sf(data = tur_luti, col='black', fill=NA) +
  theme_void()

# export Rptc_urbg to geopackage
st_write(obj = Rptc_urbg,
         here::here("clean_data/Rptc_urbg.gpkg"),
         delete_layer = TRUE)

# use clip_local function
Lptc_urbg <- clip_local(Rptc_urbg)

# plot local
ggplot() + 
  geom_sf(data = Lptc_urbg, color='#3e6e12', fill='#3e6e12') +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  theme_void()

# export local to geopackage
st_write(obj = Lptc_urbg,
         here::here("clean_data/Lptc_urbg.gpkg"),
         delete_layer = TRUE)

# remove unneeded variables
#rm(list = c(", "", "", ""))

# free memory
fm  

- - - -

# use download_zips function to download unesco areas data
list_of_urls <- c("http://www.cittametropolitana.torino.it/cms/risorse/territorio/dwd/pianificazione-territoriale/ptc2/shape_tavole/tav32/siti_unesco_ppr.zip")
dest_path <- "raw_data/zipped/"
download_zips(list_of_urls)

# manual unzip - see note in PART 2 - Functions

# transform and clip to study area with function
Rptc_unesco <- process_shp("raw_data/unzipped/ptc_unesco/siti_unesco_ppr.shp")

# merge to one feature
Rptc_unesco <- st_union(Rptc_unesco)

# plot to confirm
ggplot() + 
  geom_sf(data = Rptc_unesco, color='#b37d21', fill='#b37d21') +
  geom_sf(data = zones_luti, color = 'grey', fill=NA) +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  geom_sf(data = tur_luti, col='black', fill=NA) +
  theme_void()
  
# export Rptc_unesco to geopackage
st_write(obj = Rptc_unesco,
         here::here("clean_data/Rptc_unesco.gpkg"),
         delete_layer = TRUE)

# use clip_local function
Lptc_unesco <- clip_local(Rptc_unesco)

# plot local
ggplot() + 
  geom_sf(data = Lptc_unesco, color='#b37d21', fill='#b37d21') +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  theme_void()

# export local to geopackage
st_write(obj = Lptc_unesco,
         here::here("clean_data/Lptc_unesco.gpkg"),
         delete_layer = TRUE)

# remove unneeded variables
rm(list = c("a_pct", "b_rl", "c_loc", "test"))

# free memory
fm   

- - - -

# use download_zips function to download forest cover areas data
list_of_urls <- c("http://www.cittametropolitana.torino.it/cms/risorse/territorio/dwd/pianificazione-territoriale/ptc2/shape_tavole/tav31/copertura_forestale.zip")
dest_path <- "raw_data/zipped/"
download_zips(list_of_urls)

# manual unzip - see note in PART 2 - Functions

# transform and clip to study area with function
Rptc_fc <- process_shp("raw_data/unzipped/ptc_fc/copertura_forestale.shp")

# merge to one feature
Rptc_fc <- st_union(Rptc_fc)

# plot to confirm
ggplot() + 
  geom_sf(data = Rptc_fc, color='#729b6f', fill='#729b6f') +
  geom_sf(data = zones_luti, color = 'grey', fill=NA) +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  geom_sf(data = tur_luti, col='black', fill=NA) +
  theme_void()

# export ptc_for_area to geopackage
st_write(obj = Rptc_fc,
         here::here("clean_data/Rptc_fc.gpkg"),
         delete_layer = TRUE)

# use clip_local function
Lptc_fc <- clip_local(Rptc_fc)

# plot local
ggplot() + 
  geom_sf(data = Lptc_fc, color='#729b6f', fill='#729b6f') +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  theme_void()

# export local to geopackage
st_write(obj = Lptc_fc,
         here::here("clean_data/Lptc_fc.gpkg"),
         delete_layer = TRUE)

# remove unneeded variables
rm(list = c("a_pct", "b_rl", "c_loc", "test"))

# free memory
fm   


# PART 5 - Import & Process PAI Data ---------------------------------------------

# use download_zips function to download flood zones data
list_of_urls <- c("https://www.datigeo-piem-download.it/direct/Geoportale/RegionePiemonte/PAI/FASCE_FLUVIALI_VIGENTI.zip")
dest_path <- "raw_data/zipped/"
download_zips(list_of_urls)

# manual unzip - see note in PART 2 - Functions

# transform and clip to study area with function
  # NOTE: there are three seperate flood bands which we will merge into one
  # Future versions of this study should treat bands separately
pai_fzA <- process_shp("raw_data/unzipped/pai_fz/FASCE_FLUVIALI_VIGENTI/Fasce fluviali areali A.shp")
pai_fzB <- process_shp("raw_data/unzipped/pai_fz/FASCE_FLUVIALI_VIGENTI/Fasce fluviali areali B.shp")
pai_fzC <- process_shp("raw_data/unzipped/pai_fz/FASCE_FLUVIALI_VIGENTI/Fasce fluviali areali C.shp")

# merge bands to one feature
Rpai_fz <- st_union(pai_fzA, pai_fzB)
Rpai_fz <- st_union(Rpai_fz, pai_fzC)

# plot to confirm
ggplot() + 
  geom_sf(data = Rpai_fz, color='#9cd1ed', fill='#9cd1ed') +
  geom_sf(data = zones_luti, color = 'grey', fill=NA) +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  geom_sf(data = tur_luti, col='black', fill=NA) +
  theme_void()

# export ptc_for_area to geopackage
st_write(obj = Rpai_fz,
         here::here("clean_data/Rpai_fz.gpkg"),
         delete_layer = TRUE)

# use clip_local function
Lpai_fz <- clip_local(Rpai_fz)

# plot local
ggplot() + 
  geom_sf(data = Lpai_fz, color='#9cd1ed', fill='#9cd1ed') +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  theme_void()

# export local to geopackage
st_write(obj = Lpai_fz,
         here::here("clean_data/Lpai_fz.gpkg"),
         delete_layer = TRUE)

# remove unneeded variables
rm(list = c("pai_fzA","pai_fzB","pai_fzC"))

# free memory
fm    


# PART 6 - Import & Process BDTRE Data -----------------------------------------

# looking at the BDTRE portal and associated documentation:
# https://www.geoportale.piemonte.it/geonetwork/srv/ita/catalog.search#/metadata/r_piemon:b620d4e1-619a-4b9a-a498-1eb40ee1317b
# I determined that the following zones cover the FUA: 
# 1, 49, 50, 96, 97, 98, 99, 100, 101, 102, 103, 104, 106, 107, 108, 109, 115, 
# 117, 118, 119, 122, 123, 124, 125, 126, 127, 128, 171, 172, 173, 174, 175, 701

# there are quite a bit of files here, so we will create a folder to hold it
dir.create("raw_data/zipped/bdtre/")

# create another folder to unzip to
dir.create("raw_data/unzipped/bdtre/")

# use download_zips function to download BDTRE data
list_of_urls <- c("https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-1-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-49-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-50-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-96-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-97-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-98-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-99-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-100-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-101-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-102-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-103-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-104-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-106-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-107-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-108-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-109-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-115-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-117-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-118-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-119-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-122-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-123-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-124-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-125-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-126-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-127-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-128-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-171-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-172-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-173-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-174-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-175-EPSG32632-GPKG.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/BDTRE_STRUTTURA_AGGREGATA_2022_2ed/BDTRE_STRUTTURA_AGGREGATA_2022_2ed-GRUPPI_COMUNI-701-EPSG32632-GPKG.zip"
                 )
dest_path <- "raw_data/zipped/bdtre/"
download_zips(list_of_urls)

# NOTE: manual unzip - see note in PART 2 - Functions

# the bdtre files are geopackages so we will list all the gpkgs within the directory
# including sub directories (recursive = TRUE)
files <- list.files("raw_data/unzipped/bdtre", pattern = "*gpkg", recursive = TRUE, full.names = TRUE)

# after looking through the documentation online, and doing some translations
# I was able to determine the following:
    # the "cp_suolo" layer in the geopackage is 'ground cover' data
    # the "ty_gruppo" column in the "cp_suolo" layer is 'cover type' data
    # lastly, in the "ty_gruppo" column:
        # road cover = viab_intersezione, viab_manufatto, viab_rurale, and viab_veicolare
        # rail cover = viab_ferrovia
        # water cover = acque_corso and acque_specchio

# the following three functions will clean the road, rail, and water data, 
# merge them each into separate objects, transform the crs, and then ensure 
# they are clipped to the study area

# function to clean road data
# arguments:
# i <- the list item to filter
road_FUN <- function(i){
  st_read(i, layer='cp_suolo') %>%
    dplyr::select(ty_gruppo) %>%
    filter(str_detect(ty_gruppo, 'viab_intersezione|viab_manufatto|viab_rurale|viab_veicolare')) %>% 
    st_union(.) %>% 
    st_transform(., common_crs) %>% 
    st_make_valid(.) %>%
    st_intersection(., tur_luti)
}

# function to clean rail data
# arguments:
# i <- the list item to filter
rail_FUN <- function(i){
  st_read(i, layer='cp_suolo') %>%
    dplyr::select(ty_gruppo) %>%
    filter(str_detect(ty_gruppo, 'viab_ferrovia')) %>% 
    st_union(.) %>% 
    st_transform(., common_crs) %>% 
    st_make_valid(.) %>%
    st_intersection(., tur_luti)
}

# function to clean water data
# arguments:
# i <- the list item to filter
wtr_FUN <- function(i){
  st_read(i, layer='cp_suolo') %>%
    dplyr::select(ty_gruppo) %>%
    filter(str_detect(ty_gruppo, 'acque_corso|acque_specchio')) %>% 
    st_union(.) %>% 
    st_transform(., common_crs) %>% 
    st_make_valid(.) %>%
    st_intersection(., tur_luti)
}

# check length of list 
print(length(files))

# 33 total

# remove unneeded variables
#rm(list = c("","",""))

# free memory
fm  

- - - -

# use function to clean the road data for all 33 list items
# NOTE FOR LATER: a loop would be more efficient/elegant here
i = files[1]
road1 <- road_FUN(i)
i = files[2]
road2 <- road_FUN(i)
i = files[3]
road3 <- road_FUN(i)
i = files[4]
road4 <- road_FUN(i)
i = files[5]
road5 <- road_FUN(i)
i = files[6]
road6 <- road_FUN(i)
i = files[7]
road7 <- road_FUN(i)
i = files[8]
road8 <- road_FUN(i)
i = files[9]
road9 <- road_FUN(i)
i = files[10]
road10 <- road_FUN(i)
i = files[11]
road11 <- road_FUN(i)
i = files[12]
road12 <- road_FUN(i)
i = files[13]
road13 <- road_FUN(i)
i = files[14]
road14 <- road_FUN(i)
i = files[15]
road15 <- road_FUN(i)
i = files[16]
road16 <- road_FUN(i)
i = files[17]
road17 <- road_FUN(i)
i = files[18]
road18 <- road_FUN(i)
i = files[19]
road19 <- road_FUN(i)
i = files[20]
road20 <- road_FUN(i)
i = files[21]
road21 <- road_FUN(i)
i = files[22]
road22 <- road_FUN(i)
i = files[23]
road23 <- road_FUN(i)
i = files[24]
road24 <- road_FUN(i)
i = files[25]
road25 <- road_FUN(i)
i = files[26]
road26 <- road_FUN(i)
i = files[27]
road27 <- road_FUN(i)
i = files[28]
road28 <- road_FUN(i)
i = files[29]
road29 <- road_FUN(i)
i = files[30]
road30 <- road_FUN(i)
i = files[31]
road31 <- road_FUN(i)
i = files[32]
road32 <- road_FUN(i)
i = files[33]
road33 <- road_FUN(i)

# create a list of all road features
all_roads <- list(road1, road2, road3, road4, road5, road6, road7, road8, road9, 
               road10, road11, road12, road13, road14, road15, road16, road17, 
               road18, road19, road20, road21, road22, road23, road24, road25, 
               road26, road27, road28, road29, road30, road31, road32, road33)

# Merge the list into a single feature
Rbdtre_road <- do.call(rbind, lapply(all_roads, st_sf))
Rbdtre_road <- st_union(Rbdtre_road)

# confirm with a plot
ggplot() + 
  geom_sf(data = Rbdtre_road, color='grey', fill='grey') +
  geom_sf(data = zones_luti, color = 'grey', fill=NA) +
  geom_sf(data = tur_luti, col='black', fill=NA) +
  theme_void()

# export to geopackage
st_write(obj = Rbdtre_road, here::here("clean_data/Rbdtre_road.gpkg"), delete_layer = TRUE)

# use clip_local function
Lbdtre_road <- clip_local(Rbdtre_road)

# plot local
ggplot() + 
  geom_sf(data = Lbdtre_road, color='grey', fill='grey') +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  theme_void()

# export local to geopackage
st_write(obj = Lbdtre_road,
         here::here("clean_data/Lbdtre_road.gpkg"),
         delete_layer = TRUE)

# remove unneeded variables
rm(list = c("all_roads","road1", "road2", "road3", "road4", "road5", "road6", 
            "road7", "road8", "road9", "road10", "road11", "road12", "road13", 
            "road14", "road15", "road16", "road17", "road18", "road19", "road20", 
            "road21", "road22", "road23", "road24", "road25", "road26", "road27", 
            "road28", "road29", "road30", "road31", "road32", "road33"))

# free memory
fm   

- - - -
  
# use function to clean the rail data for all 33 list items
# NOTE FOR LATER: a loop would be more efficient/elegant here
i = files[1]
rail1 <- rail_FUN(i)
i = files[2]
rail2 <- rail_FUN(i)
i = files[3]
rail3 <- rail_FUN(i)
i = files[4]
rail4 <- rail_FUN(i)
i = files[5]
rail5 <- rail_FUN(i)
i = files[6]
rail6 <- rail_FUN(i)
i = files[7]
rail7 <- rail_FUN(i)
i = files[8]
rail8 <- rail_FUN(i)
i = files[9]
rail9 <- rail_FUN(i)
i = files[10]
rail10 <- rail_FUN(i)
i = files[11]
rail11 <- rail_FUN(i)
i = files[12]
rail12 <- rail_FUN(i)
i = files[13]
rail13 <- rail_FUN(i)
i = files[14]
rail14 <- rail_FUN(i)
i = files[15]
rail15 <- rail_FUN(i)
i = files[16]
rail16 <- rail_FUN(i)
i = files[17]
rail17 <- rail_FUN(i)
i = files[18]
rail18 <- rail_FUN(i)
i = files[19]
rail19 <- rail_FUN(i)
i = files[20]
rail20 <- rail_FUN(i)
i = files[21]
rail21 <- rail_FUN(i)
i = files[22]
rail22 <- rail_FUN(i)
i = files[23]
rail23 <- rail_FUN(i)
i = files[24]
rail24 <- rail_FUN(i)
i = files[25]
rail25 <- rail_FUN(i)
i = files[26]
rail26 <- rail_FUN(i)
i = files[27]
rail27 <- rail_FUN(i)
i = files[28]
rail28 <- rail_FUN(i)
i = files[29]
rail29 <- rail_FUN(i)
i = files[30]
rail30 <- rail_FUN(i)
i = files[31]
rail31 <- rail_FUN(i)
i = files[32]
rail32 <- rail_FUN(i)
i = files[33]
rail33 <- rail_FUN(i)

# create a list of all rail features
all_rails <- list(rail1, rail2, rail3, rail4, rail5, rail6, rail7, rail8, rail9, 
                  rail10, rail11, rail12, rail13, rail14, rail15, rail16, rail17, 
                  rail18, rail19, rail20, rail21, rail22, rail23, rail24, rail25, 
                  rail26, rail27, rail28, rail29, rail30, rail31, rail32, rail33)

# Merge the list into a single feature
Rbdtre_rail <- do.call(rbind, lapply(all_rails, st_sf))
Rbdtre_rail <- st_union(Rbdtre_rail)

# confirm with a plot
ggplot() + 
  geom_sf(data = Rbdtre_rail, color='black', fill='black') +
  geom_sf(data = zones_luti, color = 'grey', fill=NA) +
  geom_sf(data = tur_luti, col='black', fill=NA) +
  theme_void()

# export each to geopackage
st_write(obj = Rbdtre_rail, here::here("clean_data/Rbdtre_rail.gpkg"), delete_layer = TRUE)

# use clip_local function
Lbdtre_rail <- clip_local(Rbdtre_rail)

# plot local
ggplot() + 
  geom_sf(data = Lbdtre_rail, color='black', fill='black') +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  theme_void()

# export local to geopackage
st_write(obj = Lbdtre_rail,
         here::here("clean_data/Lbdtre_rail.gpkg"),
         delete_layer = TRUE)

rm(list = c("all_rails","rail1", "rail2", "rail3", "rail4", "rail5", "rail6", 
            "rail7", "rail8", "rail9", "rail10", "rail11", "rail12", "rail13", 
            "rail14", "rail15", "rail16", "rail17", "rail18", "rail19", "rail20", 
            "rail21", "rail22", "rail23", "rail24", "rail25", "rail26", "rail27", 
            "rail28", "rail29", "rail30", "rail31", "rail32", "rail33"))

# free memory
fm    

- - - -
  
# use function to clean the water data for all 33 list items
# NOTE FOR LATER: a loop would be more efficient/elegant here
i = files[1]
wtr1 <- wtr_FUN(i)
i = files[2]
wtr2 <- wtr_FUN(i)
i = files[3]
wtr3 <- wtr_FUN(i)
i = files[4]
wtr4 <- wtr_FUN(i)
i = files[5]
wtr5 <- wtr_FUN(i)
i = files[6]
wtr6 <- wtr_FUN(i)
i = files[7]
wtr7 <- wtr_FUN(i)
i = files[8]
wtr8 <- wtr_FUN(i)
i = files[9]
wtr9 <- wtr_FUN(i)
i = files[10]
wtr10 <- wtr_FUN(i)
i = files[11]
wtr11 <- wtr_FUN(i)
i = files[12]
wtr12 <- wtr_FUN(i)
i = files[13]
wtr13 <- wtr_FUN(i)
i = files[14]
wtr14 <- wtr_FUN(i)
i = files[15]
wtr15 <- wtr_FUN(i)
i = files[16]
wtr16 <- wtr_FUN(i)
i = files[17]
wtr17 <- wtr_FUN(i)
i = files[18]
wtr18 <- wtr_FUN(i)
i = files[19]
wtr19 <- wtr_FUN(i)
i = files[20]
wtr20 <- wtr_FUN(i)
i = files[21]
wtr21 <- wtr_FUN(i)
i = files[22]
wtr22 <- wtr_FUN(i)
i = files[23]
wtr23 <- wtr_FUN(i)
i = files[24]
wtr24 <- wtr_FUN(i)
i = files[25]
wtr25 <- wtr_FUN(i)
i = files[26]
wtr26 <- wtr_FUN(i)
i = files[27]
wtr27 <- wtr_FUN(i)
i = files[28]
wtr28 <- wtr_FUN(i)
i = files[29]
wtr29 <- wtr_FUN(i)
i = files[30]
wtr30 <- wtr_FUN(i)
i = files[31]
wtr31 <- wtr_FUN(i)
i = files[32]
wtr32 <- wtr_FUN(i)
i = files[33]
wtr33 <- wtr_FUN(i)

# create a list of all water features
all_wtr <- list(wtr1, wtr2, wtr3, wtr4, wtr5, wtr6, wtr7, wtr8, wtr9, 
                wtr10, wtr11, wtr12, wtr13, wtr14, wtr15, wtr16, wtr17, 
                wtr18, wtr19, wtr20, wtr21, wtr22, wtr23, wtr24, wtr25, 
                wtr26, wtr27, wtr28, wtr29, wtr30, wtr31, wtr32, wtr33)

# Merge the list into a single feature
Rbdtre_wtr <- do.call(rbind, lapply(all_wtr, st_sf))
#Rbdtre_wtr <- st_union(Rbdtre_wtr)    # not sure why, but could not export with this

# confirm with a plot
ggplot() + 
  geom_sf(data = Rbdtre_wtr, color='#2b73df', fill='#2b73df') +
  geom_sf(data = zones_luti, color = 'grey', fill=NA) +
  geom_sf(data = tur_luti, col='black', fill=NA) +
  theme_void()

# export each to geopackage
st_write(obj = Rbdtre_wtr, here::here("clean_data/Rbdtre_wtr.gpkg"), delete_layer = TRUE)

# use clip_local function
Lbdtre_wtr <- clip_local(Rbdtre_wtr)

# plot local
ggplot() + 
  geom_sf(data = Lbdtre_wtr, color='#2b73df', fill='#2b73df') +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  theme_void()

# export local to geopackage
st_write(obj = Lbdtre_wtr,
         here::here("clean_data/Lbdtre_wtr.gpkg"),
         delete_layer = TRUE)

rm(list = c("all_wtr","wtr1", "wtr2", "wtr3", "wtr4", "wtr5", "wtr6", 
            "wtr7", "wtr8", "wtr9", "wtr10", "wtr11", "wtr12", "wtr13", 
            "wtr14", "wtr15", "wtr16", "wtr17", "wtr18", "wtr19", "wtr20", 
            "wtr21", "wtr22", "wtr23", "wtr24", "wtr25", "wtr26", "wtr27", 
            "wtr28", "wtr29", "wtr30", "wtr31", "wtr32", "wtr33"))

# free memory
fm     


# PART 7 - Import & Process PRG Data ---------------------------------------------


# looking at the PRG portal and associated documentation:
# http://www.cittametropolitana.torino.it/cms/sit-cartografico/politiche-trasformaz-territ/mosaicatura-prg/atlante-dinamico
# http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/pdf/USC_metadato.pdf
# I determined that the following zones cover the FUA: 
# 1002, 1006, 1008, 1013, 1018, 1024, 1028, 1033, 1034, 1038, 1045, 1046, 1048, 
# 1051, 1053, 1055, 1058, 1060, 1062, 1063, 1064, 1065, 1068, 1078, 1082, 1085, 
# 1090, 1104, 1106, 1109, 1112, 1116, 1120, 1126, 1127, 1129, 1130, 1135, 1144, 
# 1156, 1158, 1161, 1162, 1164, 1168, 1171, 1174, 1180, 1183, 1189, 1192, 1193, 
# 1194, 1211, 1213, 1214, 1218, 1219, 1220, 1228, 1229, 1236, 1240, 1241, 1243, 
# 1248, 1249, 1252, 1253, 1262, 1265, 1272, 1273, 1276, 1280, 1284, 1286, 1289, 
# 1292, 1293, 1302, 1303, 1308, 1309, 1310, 1314, 1315, 1316

# even more files here then last time, create a folder to download to
dir.create("raw_data/zipped/prg/")

# create another folder to unzip to
dir.create("raw_data/unzipped/prg/")

# use download_zips function to download prg data
list_of_urls <- c("http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1002.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1006.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1008.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1013.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1018.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1024.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1028.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1033.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1034.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1038.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1045.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1046.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1048.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1051.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1053.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1055.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1058.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1060.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1062.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1063.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1064.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1065.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1068.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1078.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1082.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1085.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1090.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1097.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1099.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1104.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1106.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1109.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1112.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1116.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1120.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1126.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1127.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1129.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1130.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1135.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1144.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1156.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1158.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1161.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1162.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1164.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1168.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1171.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1174.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1180.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1183.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1189.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1192.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1193.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1194.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1211.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1213.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1214.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1218.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1219.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1220.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1228.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1229.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1236.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1240.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1241.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1243.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1248.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1249.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1252.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1253.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1262.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1265.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1272.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1273.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1276.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1280.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1284.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1286.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1289.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1292.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1293.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1302.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1303.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1308.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1309.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1310.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1314.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1315.zip",
                 "http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/gml/1316.zip"
                 )
dest_path <- "raw_data/zipped/prg/"
download_zips(list_of_urls)

# NOTE: manual unzip - see note in PART 2 - Functions

# the prg files are gml files so we will list all the gml files within the directory
# including sub directories (recursive = TRUE)
files <- list.files("raw_data/unzipped/prg", pattern = "*gml", recursive = TRUE, full.names = TRUE)

# after looking through the documentation online, and doing some translations
# http://www.cittametropolitana.torino.it/cms/risorse/sit-cartografico/dwd/atlante/dinamico/pdf/USC_metadato.pdf
# I was able to determine the following:
    # the "DECODIFICA" (decoding) column is the land use code information
    # in the "DECODIFICA" column:
        # industrial land uses = 'Produttivo'
        # parks and gardens land uses = 'orti urbani' and 'parchi'
        # cemetery land uses = 'cimiteri' and 'cimiteriali'
        # education land uses = 'istruzione prescolare', 'istruzione superiore', and 'post-universitaria'
        # health land uses = 'ospedaliera' and 'ospedalieri'

# the following five functions will clean the industrial (Rprg_ind), 
# parks and gardens (Rprg_png), cemetery (Rprg_cem), education (Rprg_edu), and 
# health data (Rprg_hlth), merge them each into separate objects, transform the crs, 
# and then ensure they are clipped to the study area

# function to clean Rprg_ind
# arguments:
# i <- the list item to filter
ind_FUN <- function(i){
  st_read(i) %>%
    dplyr::select(DECODIFICA) %>%
    filter(str_detect(DECODIFICA, 'Produttivo')) %>% 
    st_union(.) %>% 
    st_transform(., common_crs) %>% 
    st_make_valid(.) %>%
    st_intersection(., tur_luti)
}

# function to clean Rprg_png
# arguments:
# i <- the list item to filter
png_FUN <- function(i){
  st_read(i) %>%
    dplyr::select(DECODIFICA) %>%
    filter(str_detect(DECODIFICA, 'orti urbani|parchi')) %>% 
    st_union(.) %>% 
    st_transform(., common_crs) %>% 
    st_make_valid(.) %>%
    st_intersection(., tur_luti)
}

# function to clean Rprg_cem
# arguments:
# i <- the list item to filter
cem_FUN <- function(i){
  st_read(i) %>%
    dplyr::select(DECODIFICA) %>%
    filter(str_detect(DECODIFICA, 'cimiteri|cimiteriali')) %>% 
    st_union(.) %>% 
    st_transform(., common_crs) %>% 
    st_make_valid(.) %>%
    st_intersection(., tur_luti)
}

# function to clean Rprg_edu
# arguments:
# i <- the list item to filter
edu_FUN <- function(i){
  st_read(i) %>%
    dplyr::select(DECODIFICA) %>%
    filter(str_detect(DECODIFICA, 'istruzione prescolare|istruzione superiore|post-universitaria')) %>% 
    st_union(.) %>% 
    st_transform(., common_crs) %>% 
    st_make_valid(.) %>%
    st_intersection(., tur_luti)
}

# function to clean Rprg_hlth
# arguments:
# i <- the list item to filter
hlth_FUN <- function(i){
  st_read(i) %>%
    dplyr::select(DECODIFICA) %>%
    filter(str_detect(DECODIFICA, 'ospedaliera|ospedalieri')) %>% 
    st_union(.) %>% 
    st_transform(., common_crs) %>% 
    st_make_valid(.) %>%
    st_intersection(., tur_luti)
}

# check length of list 
print(length(files))

# 90 total

# remove unneeded variables
rm(list = c("a_pct", "b_rl", "c_loc", "test"))

# free memory
fm  


- - - -
  

# use function to clean the Rprg_ind data for all 90 list items
# NOTE FOR LATER: a loop would be more efficient/elegant here
i = files[1]
ind1 <- ind_FUN(i)
i = files[2]
ind2 <- ind_FUN(i)
i = files[3]
ind3 <- ind_FUN(i)
i = files[4]
ind4 <- ind_FUN(i)
i = files[5]
ind5 <- ind_FUN(i)
i = files[6]
ind6 <- ind_FUN(i)
i = files[7]
ind7 <- ind_FUN(i)
i = files[8]
ind8 <- ind_FUN(i)
i = files[9]
ind9 <- ind_FUN(i)
i = files[10]
ind10 <- ind_FUN(i)
i = files[11]
ind11 <- ind_FUN(i)
i = files[12]
ind12 <- ind_FUN(i)
i = files[13]
ind13 <- ind_FUN(i)
i = files[14]
ind14 <- ind_FUN(i)
i = files[15]
ind15 <- ind_FUN(i)
i = files[16]
ind16 <- ind_FUN(i)
i = files[17]
ind17 <- ind_FUN(i)
i = files[18]
ind18 <- ind_FUN(i)
i = files[19]
ind19 <- ind_FUN(i)
i = files[20]
ind20 <- ind_FUN(i)
i = files[21]
ind21 <- ind_FUN(i)
i = files[22]
ind22 <- ind_FUN(i)
i = files[23]
ind23 <- ind_FUN(i)
i = files[24]
ind24 <- ind_FUN(i)
i = files[25]
ind25 <- ind_FUN(i)
i = files[26]
ind26 <- ind_FUN(i)
i = files[27]
ind27 <- ind_FUN(i)
i = files[28]
ind28 <- ind_FUN(i)
i = files[29]
ind29 <- ind_FUN(i)
i = files[30]
ind30 <- ind_FUN(i)
i = files[31]
ind31 <- ind_FUN(i)
i = files[32]
ind32 <- ind_FUN(i)
i = files[33]
ind33 <- ind_FUN(i)
i = files[34]
ind34 <- ind_FUN(i)
i = files[35]
ind35 <- ind_FUN(i)
i = files[36]
ind36 <- ind_FUN(i)
i = files[37]
ind37 <- ind_FUN(i)
i = files[38]
ind38 <- ind_FUN(i)
i = files[39]
ind39 <- ind_FUN(i)
i = files[40]
ind40 <- ind_FUN(i)
i = files[41]
ind41 <- ind_FUN(i)
i = files[42]
ind42 <- ind_FUN(i)
i = files[43]
ind43 <- ind_FUN(i)
i = files[44]
ind44 <- ind_FUN(i)
i = files[45]
ind45 <- ind_FUN(i)
i = files[46]
ind46 <- ind_FUN(i)
i = files[47]
ind47 <- ind_FUN(i)
i = files[48]
ind48 <- ind_FUN(i)
i = files[49]
ind49 <- ind_FUN(i)
i = files[50]
ind50 <- ind_FUN(i)
i = files[51]
ind51 <- ind_FUN(i)
i = files[52]
ind52 <- ind_FUN(i)
i = files[53]
ind53 <- ind_FUN(i)
i = files[54]
ind54 <- ind_FUN(i)
i = files[55]
ind55 <- ind_FUN(i)
i = files[56]
ind56 <- ind_FUN(i)
i = files[57]
ind57 <- ind_FUN(i)
i = files[58]
ind58 <- ind_FUN(i)
i = files[59]
ind59 <- ind_FUN(i)
i = files[60]
ind60 <- ind_FUN(i)
i = files[61]
ind61 <- ind_FUN(i)
i = files[62]
ind62 <- ind_FUN(i)
i = files[63]
ind63 <- ind_FUN(i)
i = files[64]
ind64 <- ind_FUN(i)
i = files[65]
ind65 <- ind_FUN(i)
i = files[66]
ind66 <- ind_FUN(i)
i = files[67]
ind67 <- ind_FUN(i)
i = files[68]
ind68 <- ind_FUN(i)
i = files[69]
ind69 <- ind_FUN(i)
i = files[70]
ind70 <- ind_FUN(i)
i = files[71]
ind71 <- ind_FUN(i)
i = files[72]
ind72 <- ind_FUN(i)
i = files[73]
ind73 <- ind_FUN(i)
i = files[74]
ind74 <- ind_FUN(i)
i = files[75]
ind75 <- ind_FUN(i)
i = files[76]
ind76 <- ind_FUN(i)
i = files[77]
ind77 <- ind_FUN(i)
i = files[78]
ind78 <- ind_FUN(i)
i = files[79]
ind79 <- ind_FUN(i)
i = files[80]
ind80 <- ind_FUN(i)
i = files[81]
ind81 <- ind_FUN(i)
i = files[82]
ind82 <- ind_FUN(i)
i = files[83]
ind83 <- ind_FUN(i)
i = files[84]
ind84 <- ind_FUN(i)
i = files[85]
ind85 <- ind_FUN(i)
i = files[86]
ind86 <- ind_FUN(i)
i = files[87]
ind87 <- ind_FUN(i)
i = files[88]
ind88 <- ind_FUN(i)
i = files[89]
ind89 <- ind_FUN(i)
i = files[90]
ind90 <- ind_FUN(i)

# create a list of all water features
all_ind <- list(ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, ind9, ind10, 
                ind11, ind12, ind13, ind14, ind15, ind16, ind17, ind18, ind19, 
                ind20, ind21, ind22, ind23, ind24, ind25, ind26, ind27, ind28, 
                ind29, ind30, ind31, ind32, ind33, ind34, ind35, ind36, ind37,
                ind38, ind39, ind40, ind41, ind42, ind43, ind44, ind45, ind46, 
                ind47, ind48, ind49, ind50, ind51, ind52, ind53, ind54, ind55, 
                ind56, ind57, ind58, ind59, ind60, ind61, ind62, ind63, ind64,
                ind65, ind66, ind67, ind68, ind69, ind70, ind71, ind72, ind73,
                ind74, ind75, ind76, ind77, ind78, ind79, ind80, ind81, ind82,
                ind83, ind84, ind85, ind86, ind87, ind88, ind89, ind90)

# Merge the list into a single feature
Rprg_ind <- do.call(rbind, lapply(all_ind, st_sf))
Rprg_ind <- st_union(Rprg_ind)

# confirm with a plot
ggplot() + 
  geom_sf(data = Rprg_ind, color='#cdb9ef', fill='#cdb9ef') +
  geom_sf(data = zones_luti, color = 'grey', fill=NA) +
  geom_sf(data = tur_luti, col='black', fill=NA) +
  theme_void()

# export each to geopackage
st_write(obj = Rprg_ind, here::here("clean_data/Rprg_ind.gpkg"), delete_layer = TRUE)

# use clip_local function
Lprg_ind <- clip_local(Rprg_ind)

# plot local
ggplot() + 
  geom_sf(data = Lprg_ind, color='#cdb9ef', fill='#cdb9ef') +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  theme_void()

# export local to geopackage
st_write(obj = Lprg_ind,
         here::here("clean_data/Lprg_ind.gpkg"),
         delete_layer = TRUE)

rm(list = c("all_ind","ind1", "ind2", "ind3", "ind4", "ind5", "ind6", 
            "ind7", "ind8", "ind9", "ind10", "ind11", "ind12", "ind13", 
            "ind14", "ind15", "ind16", "ind17", "ind18", "ind19", "ind20", 
            "ind21", "ind22", "ind23", "ind24", "ind25", "ind26", "ind27", 
            "ind28", "ind29", "ind30", "ind31", "ind32", "ind33", "ind34", 
            "ind35", "ind36", "ind37", "ind38", "ind39", "ind40", "ind41", 
            "ind42", "ind43", "ind44", "ind45", "ind46", "ind47", "ind48", 
            "ind49", "ind50", "ind51", "ind52", "ind53", "ind54", "ind55", 
            "ind56", "ind57", "ind58", "ind59", "ind60", "ind61", "ind62", 
            "ind63", "ind64", "ind65", "ind66", "ind67", "ind68", "ind69", 
            "ind70", "ind71", "ind72", "ind73", "ind74", "ind75", "ind76", 
            "ind77", "ind78", "ind79", "ind80", "ind81", "ind82", "ind83", 
            "ind84", "ind85", "ind86", "ind87", "ind88", "ind89", "ind90"))

# free memory
fm      


- - - -
  
  
# use function to clean the Rprg_png data for all 90 list items
# NOTE FOR LATER: a loop would be more efficient/elegant here
i = files[1]
png1 <- png_FUN(i)
i = files[2]
png2 <- png_FUN(i)
i = files[3]
png3 <- png_FUN(i)
i = files[4]
png4 <- png_FUN(i)
i = files[5]
png5 <- png_FUN(i)
i = files[6]
png6 <- png_FUN(i)
i = files[7]
png7 <- png_FUN(i)
i = files[8]
png8 <- png_FUN(i)
i = files[9]
png9 <- png_FUN(i)
i = files[10]
png10 <- png_FUN(i)
i = files[11]
png11 <- png_FUN(i)
i = files[12]
png12 <- png_FUN(i)
i = files[13]
png13 <- png_FUN(i)
i = files[14]
png14 <- png_FUN(i)
i = files[15]
png15 <- png_FUN(i)
i = files[16]
png16 <- png_FUN(i)
i = files[17]
png17 <- png_FUN(i)
i = files[18]
png18 <- png_FUN(i)
i = files[19]
png19 <- png_FUN(i)
i = files[20]
png20 <- png_FUN(i)
i = files[21]
png21 <- png_FUN(i)
i = files[22]
png22 <- png_FUN(i)
i = files[23]
png23 <- png_FUN(i)
i = files[24]
png24 <- png_FUN(i)
i = files[25]
png25 <- png_FUN(i)
i = files[26]
png26 <- png_FUN(i)
i = files[27]
png27 <- png_FUN(i)
i = files[28]
png28 <- png_FUN(i)
i = files[29]
png29 <- png_FUN(i)
i = files[30]
png30 <- png_FUN(i)
i = files[31]
png31 <- png_FUN(i)
i = files[32]
png32 <- png_FUN(i)
i = files[33]
png33 <- png_FUN(i)
i = files[34]
png34 <- png_FUN(i)
i = files[35]
png35 <- png_FUN(i)
i = files[36]
png36 <- png_FUN(i)
i = files[37]
png37 <- png_FUN(i)
i = files[38]
png38 <- png_FUN(i)
i = files[39]
png39 <- png_FUN(i)
i = files[40]
png40 <- png_FUN(i)
i = files[41]
png41 <- png_FUN(i)
i = files[42]
png42 <- png_FUN(i)
i = files[43]
png43 <- png_FUN(i)
i = files[44]
png44 <- png_FUN(i)
i = files[45]
png45 <- png_FUN(i)
i = files[46]
png46 <- png_FUN(i)
i = files[47]
png47 <- png_FUN(i)
i = files[48]
png48 <- png_FUN(i)
i = files[49]
png49 <- png_FUN(i)
i = files[50]
png50 <- png_FUN(i)
i = files[51]
png51 <- png_FUN(i)
i = files[52]
png52 <- png_FUN(i)
i = files[53]
png53 <- png_FUN(i)
i = files[54]
png54 <- png_FUN(i)
i = files[55]
png55 <- png_FUN(i)
i = files[56]
png56 <- png_FUN(i)
i = files[57]
png57 <- png_FUN(i)
i = files[58]
png58 <- png_FUN(i)
i = files[59]
png59 <- png_FUN(i)
i = files[60]
png60 <- png_FUN(i)
i = files[61]
png61 <- png_FUN(i)
i = files[62]
png62 <- png_FUN(i)
i = files[63]
png63 <- png_FUN(i)
i = files[64]
png64 <- png_FUN(i)
i = files[65]
png65 <- png_FUN(i)
i = files[66]
png66 <- png_FUN(i)
i = files[67]
png67 <- png_FUN(i)
i = files[68]
png68 <- png_FUN(i)
i = files[69]
png69 <- png_FUN(i)
i = files[70]
png70 <- png_FUN(i)
i = files[71]
png71 <- png_FUN(i)
i = files[72]
png72 <- png_FUN(i)
i = files[73]
png73 <- png_FUN(i)
i = files[74]
png74 <- png_FUN(i)
i = files[75]
png75 <- png_FUN(i)
i = files[76]
png76 <- png_FUN(i)
i = files[77]
png77 <- png_FUN(i)
i = files[78]
png78 <- png_FUN(i)
i = files[79]
png79 <- png_FUN(i)
i = files[80]
png80 <- png_FUN(i)
i = files[81]
png81 <- png_FUN(i)
i = files[82]
png82 <- png_FUN(i)
i = files[83]
png83 <- png_FUN(i)
i = files[84]
png84 <- png_FUN(i)
i = files[85]
png85 <- png_FUN(i)
i = files[86]
png86 <- png_FUN(i)
i = files[87]
png87 <- png_FUN(i)
i = files[88]
png88 <- png_FUN(i)
i = files[89]
png89 <- png_FUN(i)
i = files[90]
png90 <- png_FUN(i)

# create a list of all water features
all_png <- list(png1, png2, png3, png4, png5, png6, png7, png8, png9, png10, 
                png11, png12, png13, png14, png15, png16, png17, png18, png19, 
                png20, png21, png22, png23, png24, png25, png26, png27, png28, 
                png29, png30, png31, png32, png33, png34, png35, png36, png37,
                png38, png39, png40, png41, png42, png43, png44, png45, png46, 
                png47, png48, png49, png50, png51, png52, png53, png54, png55, 
                png56, png57, png58, png59, png60, png61, png62, png63, png64,
                png65, png66, png67, png68, png69, png70, png71, png72, png73,
                png74, png75, png76, png77, png78, png79, png80, png81, png82,
                png83, png84, png85, png86, png87, png88, png89, png90)

# Merge the list into a single feature
Rprg_png <- do.call(rbind, lapply(all_png, st_sf))
Rprg_png <- st_union(Rprg_png)

# confirm with a plot
ggplot() + 
  geom_sf(data = Rprg_png, color='#b2df8a', fill='#b2df8a') +
  geom_sf(data = zones_luti, color = 'grey', fill=NA) +
  geom_sf(data = tur_luti, col='black', fill=NA) +
  theme_void()

# export each to geopackage
st_write(obj = Rprg_png, here::here("clean_data/Rprg_png.gpkg"), delete_layer = TRUE)

# use clip_local function
Lprg_png <- clip_local(Rprg_png)

# plot local
ggplot() + 
  geom_sf(data = Lprg_png, color='#b2df8a', fill='#b2df8a') +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  theme_void()

# export local to geopackage
st_write(obj = Lprg_png,
         here::here("clean_data/Lprg_png.gpkg"),
         delete_layer = TRUE)

rm(list = c("all_png","png1", "png2", "png3", "png4", "png5", "png6", 
            "png7", "png8", "png9", "png10", "png11", "png12", "png13", 
            "png14", "png15", "png16", "png17", "png18", "png19", "png20", 
            "png21", "png22", "png23", "png24", "png25", "png26", "png27", 
            "png28", "png29", "png30", "png31", "png32", "png33", "png34", 
            "png35", "png36", "png37", "png38", "png39", "png40", "png41", 
            "png42", "png43", "png44", "png45", "png46", "png47", "png48", 
            "png49", "png50", "png51", "png52", "png53", "png54", "png55", 
            "png56", "png57", "png58", "png59", "png60", "png61", "png62", 
            "png63", "png64", "png65", "png66", "png67", "png68", "png69", 
            "png70", "png71", "png72", "png73", "png74", "png75", "png76", 
            "png77", "png78", "png79", "png80", "png81", "png82", "png83", 
            "png84", "png85", "png86", "png87", "png88", "png89", "png90"))

# free memory
fm       


- - - -
  
  
# use function to clean the Rprg_png data for all 90 list items
# NOTE FOR LATER: a loop would be more efficient/elegant here
i = files[1]
cem1 <- cem_FUN(i)
i = files[2]
cem2 <- cem_FUN(i)
i = files[3]
cem3 <- cem_FUN(i)
i = files[4]
cem4 <- cem_FUN(i)
i = files[5]
cem5 <- cem_FUN(i)
i = files[6]
cem6 <- cem_FUN(i)
i = files[7]
cem7 <- cem_FUN(i)
i = files[8]
cem8 <- cem_FUN(i)
i = files[9]
cem9 <- cem_FUN(i)
i = files[10]
cem10 <- cem_FUN(i)
i = files[11]
cem11 <- cem_FUN(i)
i = files[12]
cem12 <- cem_FUN(i)
i = files[13]
cem13 <- cem_FUN(i)
i = files[14]
cem14 <- cem_FUN(i)
i = files[15]
cem15 <- cem_FUN(i)
i = files[16]
cem16 <- cem_FUN(i)
i = files[17]
cem17 <- cem_FUN(i)
i = files[18]
cem18 <- cem_FUN(i)
i = files[19]
cem19 <- cem_FUN(i)
i = files[20]
cem20 <- cem_FUN(i)
i = files[21]
cem21 <- cem_FUN(i)
i = files[22]
cem22 <- cem_FUN(i)
i = files[23]
cem23 <- cem_FUN(i)
i = files[24]
cem24 <- cem_FUN(i)
i = files[25]
cem25 <- cem_FUN(i)
i = files[26]
cem26 <- cem_FUN(i)
i = files[27]
cem27 <- cem_FUN(i)
i = files[28]
cem28 <- cem_FUN(i)
i = files[29]
cem29 <- cem_FUN(i)
i = files[30]
cem30 <- cem_FUN(i)
i = files[31]
cem31 <- cem_FUN(i)
i = files[32]
cem32 <- cem_FUN(i)
i = files[33]
cem33 <- cem_FUN(i)
i = files[34]
cem34 <- cem_FUN(i)
i = files[35]
cem35 <- cem_FUN(i)
i = files[36]
cem36 <- cem_FUN(i)
i = files[37]
cem37 <- cem_FUN(i)
i = files[38]
cem38 <- cem_FUN(i)
i = files[39]
cem39 <- cem_FUN(i)
i = files[40]
cem40 <- cem_FUN(i)
i = files[41]
cem41 <- cem_FUN(i)
i = files[42]
cem42 <- cem_FUN(i)
i = files[43]
cem43 <- cem_FUN(i)
i = files[44]
cem44 <- cem_FUN(i)
i = files[45]
cem45 <- cem_FUN(i)
i = files[46]
cem46 <- cem_FUN(i)
i = files[47]
cem47 <- cem_FUN(i)
i = files[48]
cem48 <- cem_FUN(i)
i = files[49]
cem49 <- cem_FUN(i)
i = files[50]
cem50 <- cem_FUN(i)
i = files[51]
cem51 <- cem_FUN(i)
i = files[52]
cem52 <- cem_FUN(i)
i = files[53]
cem53 <- cem_FUN(i)
i = files[54]
cem54 <- cem_FUN(i)
i = files[55]
cem55 <- cem_FUN(i)
i = files[56]
cem56 <- cem_FUN(i)
i = files[57]
cem57 <- cem_FUN(i)
i = files[58]
cem58 <- cem_FUN(i)
i = files[59]
cem59 <- cem_FUN(i)
i = files[60]
cem60 <- cem_FUN(i)
i = files[61]
cem61 <- cem_FUN(i)
i = files[62]
cem62 <- cem_FUN(i)
i = files[63]
cem63 <- cem_FUN(i)
i = files[64]
cem64 <- cem_FUN(i)
i = files[65]
cem65 <- cem_FUN(i)
i = files[66]
cem66 <- cem_FUN(i)
i = files[67]
cem67 <- cem_FUN(i)
i = files[68]
cem68 <- cem_FUN(i)
i = files[69]
cem69 <- cem_FUN(i)
i = files[70]
cem70 <- cem_FUN(i)
i = files[71]
cem71 <- cem_FUN(i)
i = files[72]
cem72 <- cem_FUN(i)
i = files[73]
cem73 <- cem_FUN(i)
i = files[74]
cem74 <- cem_FUN(i)
i = files[75]
cem75 <- cem_FUN(i)
i = files[76]
cem76 <- cem_FUN(i)
i = files[77]
cem77 <- cem_FUN(i)
i = files[78]
cem78 <- cem_FUN(i)
i = files[79]
cem79 <- cem_FUN(i)
i = files[80]
cem80 <- cem_FUN(i)
i = files[81]
cem81 <- cem_FUN(i)
i = files[82]
cem82 <- cem_FUN(i)
i = files[83]
cem83 <- cem_FUN(i)
i = files[84]
cem84 <- cem_FUN(i)
i = files[85]
cem85 <- cem_FUN(i)
i = files[86]
cem86 <- cem_FUN(i)
i = files[87]
cem87 <- cem_FUN(i)
i = files[88]
cem88 <- cem_FUN(i)
i = files[89]
cem89 <- cem_FUN(i)
i = files[90]
cem90 <- cem_FUN(i)

# create a list of all water features
all_cem <- list(cem1, cem2, cem3, cem4, cem5, cem6, cem7, cem8, cem9, cem10, 
                cem11, cem12, cem13, cem14, cem15, cem16, cem17, cem18, cem19, 
                cem20, cem21, cem22, cem23, cem24, cem25, cem26, cem27, cem28, 
                cem29, cem30, cem31, cem32, cem33, cem34, cem35, cem36, cem37,
                cem38, cem39, cem40, cem41, cem42, cem43, cem44, cem45, cem46, 
                cem47, cem48, cem49, cem50, cem51, cem52, cem53, cem54, cem55, 
                cem56, cem57, cem58, cem59, cem60, cem61, cem62, cem63, cem64,
                cem65, cem66, cem67, cem68, cem69, cem70, cem71, cem72, cem73,
                cem74, cem75, cem76, cem77, cem78, cem79, cem80, cem81, cem82,
                cem83, cem84, cem85, cem86, cem87, cem88, cem89, cem90)

# Merge the list into a single feature
Rprg_cem <- do.call(rbind, lapply(all_cem, st_sf))
Rprg_cem <- st_union(Rprg_cem)

# confirm with a plot
ggplot() + 
  geom_sf(data = Rprg_cem, color='#557a52', fill='#557a52') +
  geom_sf(data = zones_luti, color = 'grey', fill=NA) +
  geom_sf(data = tur_luti, col='black', fill=NA) +
  theme_void()

# export each to geopackage
st_write(obj = Rprg_cem, here::here("clean_data/Rprg_cem.gpkg"), delete_layer = TRUE)

# use clip_local function
Lprg_cem <- clip_local(Rprg_cem)

# plot local
ggplot() + 
  geom_sf(data = Lprg_cem, color='#557a52', fill='#557a52') +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  theme_void()

# export local to geopackage
st_write(obj = Lprg_cem,
         here::here("clean_data/Lprg_cem.gpkg"),
         delete_layer = TRUE)

rm(list = c("all_cem","cem1", "cem2", "cem3", "cem4", "cem5", "cem6", 
            "cem7", "cem8", "cem9", "cem10", "cem11", "cem12", "cem13", 
            "cem14", "cem15", "cem16", "cem17", "cem18", "cem19", "cem20", 
            "cem21", "cem22", "cem23", "cem24", "cem25", "cem26", "cem27", 
            "cem28", "cem29", "cem30", "cem31", "cem32", "cem33", "cem34", 
            "cem35", "cem36", "cem37", "cem38", "cem39", "cem40", "cem41", 
            "cem42", "cem43", "cem44", "cem45", "cem46", "cem47", "cem48", 
            "cem49", "cem50", "cem51", "cem52", "cem53", "cem54", "cem55", 
            "cem56", "cem57", "cem58", "cem59", "cem60", "cem61", "cem62", 
            "cem63", "cem64", "cem65", "cem66", "cem67", "cem68", "cem69", 
            "cem70", "cem71", "cem72", "cem73", "cem74", "cem75", "cem76", 
            "cem77", "cem78", "cem79", "cem80", "cem81", "cem82", "cem83", 
            "cem84", "cem85", "cem86", "cem87", "cem88", "cem89", "cem90"))

# free memory
fm      


- - - -
  
  
# use function to clean the Rprg_edu data for all 90 list items
# NOTE FOR LATER: a loop would be more efficient/elegant here
i = files[1]
edu1 <- edu_FUN(i)
i = files[2]
edu2 <- edu_FUN(i)
i = files[3]
edu3 <- edu_FUN(i)
i = files[4]
edu4 <- edu_FUN(i)
i = files[5]
edu5 <- edu_FUN(i)
i = files[6]
edu6 <- edu_FUN(i)
i = files[7]
edu7 <- edu_FUN(i)
i = files[8]
edu8 <- edu_FUN(i)
i = files[9]
edu9 <- edu_FUN(i)
i = files[10]
edu10 <- edu_FUN(i)
i = files[11]
edu11 <- edu_FUN(i)
i = files[12]
edu12 <- edu_FUN(i)
i = files[13]
edu13 <- edu_FUN(i)
i = files[14]
edu14 <- edu_FUN(i)
i = files[15]
edu15 <- edu_FUN(i)
i = files[16]
edu16 <- edu_FUN(i)
i = files[17]
edu17 <- edu_FUN(i)
i = files[18]
edu18 <- edu_FUN(i)
i = files[19]
edu19 <- edu_FUN(i)
i = files[20]
edu20 <- edu_FUN(i)
i = files[21]
edu21 <- edu_FUN(i)
i = files[22]
edu22 <- edu_FUN(i)
i = files[23]
edu23 <- edu_FUN(i)
i = files[24]
edu24 <- edu_FUN(i)
i = files[25]
edu25 <- edu_FUN(i)
i = files[26]
edu26 <- edu_FUN(i)
i = files[27]
edu27 <- edu_FUN(i)
i = files[28]
edu28 <- edu_FUN(i)
i = files[29]
edu29 <- edu_FUN(i)
i = files[30]
edu30 <- edu_FUN(i)
i = files[31]
edu31 <- edu_FUN(i)
i = files[32]
edu32 <- edu_FUN(i)
i = files[33]
edu33 <- edu_FUN(i)
i = files[34]
edu34 <- edu_FUN(i)
i = files[35]
edu35 <- edu_FUN(i)
i = files[36]
edu36 <- edu_FUN(i)
i = files[37]
edu37 <- edu_FUN(i)
i = files[38]
edu38 <- edu_FUN(i)
i = files[39]
edu39 <- edu_FUN(i)
i = files[40]
edu40 <- edu_FUN(i)
i = files[41]
edu41 <- edu_FUN(i)
i = files[42]
edu42 <- edu_FUN(i)
i = files[43]
edu43 <- edu_FUN(i)
i = files[44]
edu44 <- edu_FUN(i)
i = files[45]
edu45 <- edu_FUN(i)
i = files[46]
edu46 <- edu_FUN(i)
i = files[47]
edu47 <- edu_FUN(i)
i = files[48]
edu48 <- edu_FUN(i)
i = files[49]
edu49 <- edu_FUN(i)
i = files[50]
edu50 <- edu_FUN(i)
i = files[51]
edu51 <- edu_FUN(i)
i = files[52]
edu52 <- edu_FUN(i)
i = files[53]
edu53 <- edu_FUN(i)
i = files[54]
edu54 <- edu_FUN(i)
i = files[55]
edu55 <- edu_FUN(i)
i = files[56]
edu56 <- edu_FUN(i)
i = files[57]
edu57 <- edu_FUN(i)
i = files[58]
edu58 <- edu_FUN(i)
i = files[59]
edu59 <- edu_FUN(i)
i = files[60]
edu60 <- edu_FUN(i)
i = files[61]
edu61 <- edu_FUN(i)
i = files[62]
edu62 <- edu_FUN(i)
i = files[63]
edu63 <- edu_FUN(i)
i = files[64]
edu64 <- edu_FUN(i)
i = files[65]
edu65 <- edu_FUN(i)
i = files[66]
edu66 <- edu_FUN(i)
i = files[67]
edu67 <- edu_FUN(i)
i = files[68]
edu68 <- edu_FUN(i)
i = files[69]
edu69 <- edu_FUN(i)
i = files[70]
edu70 <- edu_FUN(i)
i = files[71]
edu71 <- edu_FUN(i)
i = files[72]
edu72 <- edu_FUN(i)
i = files[73]
edu73 <- edu_FUN(i)
i = files[74]
edu74 <- edu_FUN(i)
i = files[75]
edu75 <- edu_FUN(i)
i = files[76]
edu76 <- edu_FUN(i)
i = files[77]
edu77 <- edu_FUN(i)
i = files[78]
edu78 <- edu_FUN(i)
i = files[79]
edu79 <- edu_FUN(i)
i = files[80]
edu80 <- edu_FUN(i)
i = files[81]
edu81 <- edu_FUN(i)
i = files[82]
edu82 <- edu_FUN(i)
i = files[83]
edu83 <- edu_FUN(i)
i = files[84]
edu84 <- edu_FUN(i)
i = files[85]
edu85 <- edu_FUN(i)
i = files[86]
edu86 <- edu_FUN(i)
i = files[87]
edu87 <- edu_FUN(i)
i = files[88]
edu88 <- edu_FUN(i)
i = files[89]
edu89 <- edu_FUN(i)
i = files[90]
edu90 <- edu_FUN(i)

# create a list of all water features
all_edu <- list(edu1, edu2, edu3, edu4, edu5, edu6, edu7, edu8, edu9, edu10, 
                edu11, edu12, edu13, edu14, edu15, edu16, edu17, edu18, edu19, 
                edu20, edu21, edu22, edu23, edu24, edu25, edu26, edu27, edu28, 
                edu29, edu30, edu31, edu32, edu33, edu34, edu35, edu36, edu37,
                edu38, edu39, edu40, edu41, edu42, edu43, edu44, edu45, edu46, 
                edu47, edu48, edu49, edu50, edu51, edu52, edu53, edu54, edu55, 
                edu56, edu57, edu58, edu59, edu60, edu61, edu62, edu63, edu64,
                edu65, edu66, edu67, edu68, edu69, edu70, edu71, edu72, edu73,
                edu74, edu75, edu76, edu77, edu78, edu79, edu80, edu81, edu82,
                edu83, edu84, edu85, edu86, edu87, edu88, edu89, edu90)

# Merge the list into a single feature
Rprg_edu <- do.call(rbind, lapply(all_edu, st_sf))
Rprg_edu <- st_union(Rprg_edu)

# confirm with a plot
ggplot() + 
  geom_sf(data = Rprg_edu, color='#d2982b', fill='#d2982b') +
  geom_sf(data = zones_luti, color = 'grey', fill=NA) +
  geom_sf(data = tur_luti, col='black', fill=NA) +
  theme_void()

# export each to geopackage
st_write(obj = Rprg_edu, here::here("clean_data/Rprg_edu.gpkg"), delete_layer = TRUE)

# use clip_local function
Lprg_edu <- clip_local(Rprg_edu)

# plot local
ggplot() + 
  geom_sf(data = Lprg_edu, color='#d2982b', fill='#d2982b') +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  theme_void()

# export local to geopackage
st_write(obj = Lprg_edu,
         here::here("clean_data/Lprg_edu.gpkg"),
         delete_layer = TRUE)

rm(list = c("all_edu","edu1", "edu2", "edu3", "edu4", "edu5", "edu6", 
            "edu7", "edu8", "edu9", "edu10", "edu11", "edu12", "edu13", 
            "edu14", "edu15", "edu16", "edu17", "edu18", "edu19", "edu20", 
            "edu21", "edu22", "edu23", "edu24", "edu25", "edu26", "edu27", 
            "edu28", "edu29", "edu30", "edu31", "edu32", "edu33", "edu34", 
            "edu35", "edu36", "edu37", "edu38", "edu39", "edu40", "edu41", 
            "edu42", "edu43", "edu44", "edu45", "edu46", "edu47", "edu48", 
            "edu49", "edu50", "edu51", "edu52", "edu53", "edu54", "edu55", 
            "edu56", "edu57", "edu58", "edu59", "edu60", "edu61", "edu62", 
            "edu63", "edu64", "edu65", "edu66", "edu67", "edu68", "edu69", 
            "edu70", "edu71", "edu72", "edu73", "edu74", "edu75", "edu76", 
            "edu77", "edu78", "edu79", "edu80", "edu81", "edu82", "edu83", 
            "edu84", "edu85", "edu86", "edu87", "edu88", "edu89", "edu90"))

# free memory
fm       


- - - -
  
  
# use function to clean the Rprg_hlth data for all 90 list items
# NOTE FOR LATER: a loop would be more efficient/elegant here
i = files[1]
hlth1 <- hlth_FUN(i)
i = files[2]
hlth2 <- hlth_FUN(i)
i = files[3]
hlth3 <- hlth_FUN(i)
i = files[4]
hlth4 <- hlth_FUN(i)
i = files[5]
hlth5 <- hlth_FUN(i)
i = files[6]
hlth6 <- hlth_FUN(i)
i = files[7]
hlth7 <- hlth_FUN(i)
i = files[8]
hlth8 <- hlth_FUN(i)
i = files[9]
hlth9 <- hlth_FUN(i)
i = files[10]
hlth10 <- hlth_FUN(i)
i = files[11]
hlth11 <- hlth_FUN(i)
i = files[12]
hlth12 <- hlth_FUN(i)
i = files[13]
hlth13 <- hlth_FUN(i)
i = files[14]
hlth14 <- hlth_FUN(i)
i = files[15]
hlth15 <- hlth_FUN(i)
i = files[16]
hlth16 <- hlth_FUN(i)
i = files[17]
hlth17 <- hlth_FUN(i)
i = files[18]
hlth18 <- hlth_FUN(i)
i = files[19]
hlth19 <- hlth_FUN(i)
i = files[20]
hlth20 <- hlth_FUN(i)
i = files[21]
hlth21 <- hlth_FUN(i)
i = files[22]
hlth22 <- hlth_FUN(i)
i = files[23]
hlth23 <- hlth_FUN(i)
i = files[24]
hlth24 <- hlth_FUN(i)
i = files[25]
hlth25 <- hlth_FUN(i)
i = files[26]
hlth26 <- hlth_FUN(i)
i = files[27]
hlth27 <- hlth_FUN(i)
i = files[28]
hlth28 <- hlth_FUN(i)
i = files[29]
hlth29 <- hlth_FUN(i)
i = files[30]
hlth30 <- hlth_FUN(i)
i = files[31]
hlth31 <- hlth_FUN(i)
i = files[32]
hlth32 <- hlth_FUN(i)
i = files[33]
hlth33 <- hlth_FUN(i)
i = files[34]
hlth34 <- hlth_FUN(i)
i = files[35]
hlth35 <- hlth_FUN(i)
i = files[36]
hlth36 <- hlth_FUN(i)
i = files[37]
hlth37 <- hlth_FUN(i)
i = files[38]
hlth38 <- hlth_FUN(i)
i = files[39]
hlth39 <- hlth_FUN(i)
i = files[40]
hlth40 <- hlth_FUN(i)
i = files[41]
hlth41 <- hlth_FUN(i)
i = files[42]
hlth42 <- hlth_FUN(i)
i = files[43]
hlth43 <- hlth_FUN(i)
i = files[44]
hlth44 <- hlth_FUN(i)
i = files[45]
hlth45 <- hlth_FUN(i)
i = files[46]
hlth46 <- hlth_FUN(i)
i = files[47]
hlth47 <- hlth_FUN(i)
i = files[48]
hlth48 <- hlth_FUN(i)
i = files[49]
hlth49 <- hlth_FUN(i)
i = files[50]
hlth50 <- hlth_FUN(i)
i = files[51]
hlth51 <- hlth_FUN(i)
i = files[52]
hlth52 <- hlth_FUN(i)
i = files[53]
hlth53 <- hlth_FUN(i)
i = files[54]
hlth54 <- hlth_FUN(i)
i = files[55]
hlth55 <- hlth_FUN(i)
i = files[56]
hlth56 <- hlth_FUN(i)
i = files[57]
hlth57 <- hlth_FUN(i)
i = files[58]
hlth58 <- hlth_FUN(i)
i = files[59]
hlth59 <- hlth_FUN(i)
i = files[60]
hlth60 <- hlth_FUN(i)
i = files[61]
hlth61 <- hlth_FUN(i)
i = files[62]
hlth62 <- hlth_FUN(i)
i = files[63]
hlth63 <- hlth_FUN(i)
i = files[64]
hlth64 <- hlth_FUN(i)
i = files[65]
hlth65 <- hlth_FUN(i)
i = files[66]
hlth66 <- hlth_FUN(i)
i = files[67]
hlth67 <- hlth_FUN(i)
i = files[68]
hlth68 <- hlth_FUN(i)
i = files[69]
hlth69 <- hlth_FUN(i)
i = files[70]
hlth70 <- hlth_FUN(i)
i = files[71]
hlth71 <- hlth_FUN(i)
i = files[72]
hlth72 <- hlth_FUN(i)
i = files[73]
hlth73 <- hlth_FUN(i)
i = files[74]
hlth74 <- hlth_FUN(i)
i = files[75]
hlth75 <- hlth_FUN(i)
i = files[76]
hlth76 <- hlth_FUN(i)
i = files[77]
hlth77 <- hlth_FUN(i)
i = files[78]
hlth78 <- hlth_FUN(i)
i = files[79]
hlth79 <- hlth_FUN(i)
i = files[80]
hlth80 <- hlth_FUN(i)
i = files[81]
hlth81 <- hlth_FUN(i)
i = files[82]
hlth82 <- hlth_FUN(i)
i = files[83]
hlth83 <- hlth_FUN(i)
i = files[84]
hlth84 <- hlth_FUN(i)
i = files[85]
hlth85 <- hlth_FUN(i)
i = files[86]
hlth86 <- hlth_FUN(i)
i = files[87]
hlth87 <- hlth_FUN(i)
i = files[88]
hlth88 <- hlth_FUN(i)
i = files[89]
hlth89 <- hlth_FUN(i)
i = files[90]
hlth90 <- hlth_FUN(i)

# create a list of all water features
all_hlth <- list(hlth1, hlth2, hlth3, hlth4, hlth5, hlth6, hlth7, hlth8, hlth9, hlth10, 
                hlth11, hlth12, hlth13, hlth14, hlth15, hlth16, hlth17, hlth18, hlth19, 
                hlth20, hlth21, hlth22, hlth23, hlth24, hlth25, hlth26, hlth27, hlth28, 
                hlth29, hlth30, hlth31, hlth32, hlth33, hlth34, hlth35, hlth36, hlth37,
                hlth38, hlth39, hlth40, hlth41, hlth42, hlth43, hlth44, hlth45, hlth46, 
                hlth47, hlth48, hlth49, hlth50, hlth51, hlth52, hlth53, hlth54, hlth55, 
                hlth56, hlth57, hlth58, hlth59, hlth60, hlth61, hlth62, hlth63, hlth64,
                hlth65, hlth66, hlth67, hlth68, hlth69, hlth70, hlth71, hlth72, hlth73,
                hlth74, hlth75, hlth76, hlth77, hlth78, hlth79, hlth80, hlth81, hlth82,
                hlth83, hlth84, hlth85, hlth86, hlth87, hlth88, hlth89, hlth90)

# Merge the list into a single feature
Rprg_hlth <- do.call(rbind, lapply(all_hlth, st_sf))
Rprg_hlth <- st_union(Rprg_hlth)

# confirm with a plot
ggplot() + 
  geom_sf(data = Rprg_hlth, color='#d58687', fill='#d58687') +
  geom_sf(data = zones_luti, color = 'grey', fill=NA) +
  geom_sf(data = tur_luti, col='black', fill=NA) +
  theme_void()

# export each to geopackage
st_write(obj = Rprg_hlth, here::here("clean_data/Rprg_hlth.gpkg"), delete_layer = TRUE)

# use clip_local function
Lprg_hlth <- clip_local(Rprg_hlth)

# plot local
ggplot() + 
  geom_sf(data = Lprg_hlth, color='#d58687', fill='#d58687') +
  geom_sf(data = zones_loc, col='black', fill=NA) +
  theme_void()

# export local to geopackage
st_write(obj = Lprg_hlth,
         here::here("clean_data/Lprg_hlth.gpkg"),
         delete_layer = TRUE)

rm(list = c("all_hlth","hlth1", "hlth2", "hlth3", "hlth4", "hlth5", "hlth6", 
            "hlth7", "hlth8", "hlth9", "hlth10", "hlth11", "hlth12", "hlth13", 
            "hlth14", "hlth15", "hlth16", "hlth17", "hlth18", "hlth19", "hlth20", 
            "hlth21", "hlth22", "hlth23", "hlth24", "hlth25", "hlth26", "hlth27", 
            "hlth28", "hlth29", "hlth30", "hlth31", "hlth32", "hlth33", "hlth34", 
            "hlth35", "hlth36", "hlth37", "hlth38", "hlth39", "hlth40", "hlth41", 
            "hlth42", "hlth43", "hlth44", "hlth45", "hlth46", "hlth47", "hlth48", 
            "hlth49", "hlth50", "hlth51", "hlth52", "hlth53", "hlth54", "hlth55", 
            "hlth56", "hlth57", "hlth58", "hlth59", "hlth60", "hlth61", "hlth62", 
            "hlth63", "hlth64", "hlth65", "hlth66", "hlth67", "hlth68", "hlth69", 
            "hlth70", "hlth71", "hlth72", "hlth73", "hlth74", "hlth75", "hlth76", 
            "hlth77", "hlth78", "hlth79", "hlth80", "hlth81", "hlth82", "hlth83", 
            "hlth84", "hlth85", "hlth86", "hlth87", "hlth88", "hlth89", "hlth90"))

# free memory
fm     


# PART 8 - Import & Process LIDAR Data -------------------------------------------

# looking at the slope portal and associated documentation
# https://www.geoportale.piemonte.it/visregpigo/?action-type=dwl&url=https:%2F%2Fgeomap.reteunitaria.piemonte.it%2Fws%2Ftaims%2Frp-01%2Ftaimsscaricogp%2Fwms_scaricogp
# I determined that the following zones cover the FUA:  
# 133, 134, 135, 136, 154, 155, 156, 157, 172, 173, 174, 175

# as done previously, we create a folder to download to
dir.create("raw_data/zipped/slp/")

# and create another folder to unzip to
dir.create("raw_data/unzipped/slp/")

# use download_zips function to download slp data
list_of_urls <- c("https://www.datigeo-piem-download.it/static/regp01/DTM5_ICE/RIPRESA_AEREA_ICE_2009_2011_DTM-SDO_CTR_FOGLI50-133-EPSG32632-TIF.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/DTM5_ICE/RIPRESA_AEREA_ICE_2009_2011_DTM-SDO_CTR_FOGLI50-134-EPSG32632-TIF.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/DTM5_ICE/RIPRESA_AEREA_ICE_2009_2011_DTM-SDO_CTR_FOGLI50-135-EPSG32632-TIF.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/DTM5_ICE/RIPRESA_AEREA_ICE_2009_2011_DTM-SDO_CTR_FOGLI50-136-EPSG32632-TIF.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/DTM5_ICE/RIPRESA_AEREA_ICE_2009_2011_DTM-SDO_CTR_FOGLI50-154-EPSG32632-TIF.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/DTM5_ICE/RIPRESA_AEREA_ICE_2009_2011_DTM-SDO_CTR_FOGLI50-155-EPSG32632-TIF.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/DTM5_ICE/RIPRESA_AEREA_ICE_2009_2011_DTM-SDO_CTR_FOGLI50-156-EPSG32632-TIF.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/DTM5_ICE/RIPRESA_AEREA_ICE_2009_2011_DTM-SDO_CTR_FOGLI50-157-EPSG32632-TIF.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/DTM5_ICE/RIPRESA_AEREA_ICE_2009_2011_DTM-SDO_CTR_FOGLI50-172-EPSG32632-TIF.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/DTM5_ICE/RIPRESA_AEREA_ICE_2009_2011_DTM-SDO_CTR_FOGLI50-173-EPSG32632-TIF.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/DTM5_ICE/RIPRESA_AEREA_ICE_2009_2011_DTM-SDO_CTR_FOGLI50-174-EPSG32632-TIF.zip",
                  "https://www.datigeo-piem-download.it/static/regp01/DTM5_ICE/RIPRESA_AEREA_ICE_2009_2011_DTM-SDO_CTR_FOGLI50-175-EPSG32632-TIF.zip"
                  )
dest_path <- "raw_data/zipped/slp/"
download_zips(list_of_urls)

# NOTE: manual unzip - see note in PART 2 - Functions

# the slp files are geotiff files so we will list all the tif files within the directory
# including sub directories (recursive = TRUE)
demlist <- list.files("raw_data/unzipped/slp", pattern = "*tif", recursive = TRUE, full.names = TRUE)

# import all raster files in folder using lapply
alldems <- lapply(demlist, raster)

# merge rasters
names(alldems) <- c("x", "y")
alldems$filename <- 'dem.tif'
alldems$overwrite <- TRUE
dem <- do.call(merge, alldems)

# confirm with a plot
plot(dem)
plot(tur_luti, add = T)

# change crs
dem <- projectRaster(dem, crs = common_crs)

# confirm with a plot
plot(dem)
plot(tur_luti, add = T)

# calculate slope
dem_slp = terrain(dem, opt = "slope", unit = "degrees")

# confirm with a plot
plot(dem_slp)
plot(tur_luti, add = T)

# crop and mask the raster to the study area limits
dem_slp = crop(dem_slp, zones_luti)

# confirm with a plot
plot(dem_slp)
plot(tur_luti, add = T)

# export slp as a geotiff, and assign -9999 to missing values
writeRaster(dem_slp, "clean_data/dem_slp.tif",
            format="GTiff", 
            overwrite=TRUE, 
            progress="window",
            NAflag=-9999)
            
# free memory
fm 

# PART 9 - Constraint Plots -----------------------------------

# Constraint Layers - Region

# plot A - rs - colour
test1 <- ggplot() +
  labs(title = "Constraint Layers - Region",
       subtitle = "A1. Structure") +
  geom_sf(data = Rbdtre_wtr, color=NA, fill='#2b73df') +
  geom_sf(data = Rbdtre_rail, color=NA, fill='black') +
  geom_sf(data = Rbdtre_road, color=NA, fill=NA) +
  geom_sf(data = tur_loc, color = 'red', fill=NA, size = 0.3) +
  geom_sf(data = tur_luti, color = 'black', fill=NA, size = 0.1) +
  theme_void()

# plot B - rscp - colour
test2 <- ggplot() +
  labs(subtitle = "Conservation Policies") +
  geom_sf(data = Rppr_ca, color = 'lightgreen', fill = 'lightgreen') +
  geom_sf(data = Rptc_unesco, color='#b37d21', fill='#b37d21') +
  geom_sf(data = Rptc_urbg, color='#3e6e12', fill='#3e6e12') +
  geom_sf(data = Rptc_fc, color='#729b6f', fill='#729b6f') +
  geom_sf(data = tur_loc, color = 'red', fill=NA, size = 0.3) +
  geom_sf(data = tur_luti, color = 'black', fill=NA, size = 0.1) +
  theme_void()

# plot C - rscplup - colour
test3 <- ggplot() +
  labs(subtitle = "A3. Land Use Policies") +
  geom_sf(data = Rpai_fz, color='#9cd1ed', fill='#9cd1ed') +
  geom_sf(data = Rprg_png, color='#b2df8a', fill='#b2df8a') +
  geom_sf(data = Rprg_cem, color='#557a52', fill='#557a52') +
  geom_sf(data = Rprg_hlth, color='#d58687', fill='#d58687') +
  geom_sf(data = Rprg_edu, color='#d2982b', fill='#d2982b') +
  geom_sf(data = Rprg_ind, color='#cdb9ef', fill='#cdb9ef') +
  geom_sf(data = tur_loc, color = 'red', fill=NA, size = 0.3) +
  geom_sf(data = tur_luti, color = 'black', fill=NA, size = 0.1) +
  theme_void()

# plot D - rs - black
test4 <- ggplot() +
  labs(subtitle = "B1. Structural Constraints") +
  geom_sf(data = Rbdtre_wtr, color=NA, fill='black') +
  geom_sf(data = Rbdtre_rail, color=NA, fill='black') +
  geom_sf(data = Rbdtre_road, color=NA, fill=NA) +
  geom_sf(data = tur_loc, color = 'red', fill=NA, size = 0.3) +
  geom_sf(data = tur_luti, color = 'black', fill=NA, size = 0.1) +
  theme_void() +
  annotation_scale(location = "bl",height = unit(0.1, "cm"))

# plot E - rscp - black
test5 <- ggplot() +
  labs(subtitle = "B2. Conservation Constraints") +
  geom_sf(data = Rppr_ca, color=NA, fill='black') +
  geom_sf(data = Rptc_unesco,color=NA, fill='black') +
  geom_sf(data = Rptc_urbg, color=NA, fill='black') +
  geom_sf(data = Rptc_fc, color=NA, fill='black') +
  geom_sf(data = tur_loc, color = 'red', fill=NA, size = 0.3) +
  geom_sf(data = tur_luti, color = 'black', fill=NA, size = 0.1) +
  theme_void()

# plot F - rscplup - black
test6 <- ggplot() +
  labs(subtitle = "B3. Land Use Constraints",
       caption = "SOURCE: PTC2,2015; PAI,2021; PRG,2021; BDTRE,2022") +
  geom_sf(data = Rpai_fz, color=NA, fill='black') +
  geom_sf(data = Rprg_png,color=NA, fill='black') +
  geom_sf(data = Rprg_cem, color=NA, fill='black') +
  geom_sf(data = Rprg_hlth, color=NA, fill='black') +
  geom_sf(data = Rprg_edu, color=NA, fill='black') +
  geom_sf(data = Rprg_ind, color=NA, fill='black') +
  geom_sf(data = tur_loc, color = 'red', fill=NA, size = 0.3) +
  geom_sf(data = tur_luti, color = 'black', fill=NA, size = 0.1) +
  theme_void() +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)

# test plot together
plot_r_constraints <- plot_grid(ncol = 3,
                                test1, test2, test3, test4, test5, test6,
                                align = "hv")
plot_r_constraints

# save plot
ggsave(filename = "plot_r_constraints.png",
       device = "png",
       path = here::here("figs/"),
       width = 9,
       height = 6,
       units = "in",
       dpi = 300)


# Constraint Layers - City

# plot A - cs - colour
test1 <- ggplot() +
  labs(title = "Constraint Layers - City",
       subtitle = "A1. Structure") +
  geom_sf(data = Lbdtre_wtr, color=NA, fill='#2b73df') +
  geom_sf(data = Lbdtre_rail, color=NA, fill='black') +
  geom_sf(data = Lbdtre_road, color=NA, fill='black') +
  geom_sf(data = tur_loc, color = 'red', fill=NA, size = 0.3) +
  theme_void()

# plot B - cscp - colour
test2 <- ggplot() +
  labs(subtitle = "A2. Conservation Policies") +
  geom_sf(data = Lppr_ca, color = 'lightgreen', fill = 'lightgreen') +
  geom_sf(data = Lptc_unesco, color='#b37d21', fill='#b37d21') +
  geom_sf(data = Lptc_urbg, color='#3e6e12', fill='#3e6e12') +
  geom_sf(data = Lptc_fc, color='#729b6f', fill='#729b6f') +
  geom_sf(data = tur_loc, color = 'red', fill=NA, size = 0.3) +
  theme_void()

# plot C - cscplup - colour
test3 <- ggplot() +
  labs(subtitle = "A3. Land Use Policies") +
  geom_sf(data = Lpai_fz, color='#9cd1ed', fill='#9cd1ed') +
  geom_sf(data = Lprg_png, color='#b2df8a', fill='#b2df8a') +
  geom_sf(data = Lprg_cem, color='#557a52', fill='#557a52') +
  geom_sf(data = Lprg_hlth, color='#d58687', fill='#d58687') +
  geom_sf(data = Lprg_edu, color='#d2982b', fill='#d2982b') +
  geom_sf(data = Lprg_ind, color='#cdb9ef', fill='#cdb9ef') +
  geom_sf(data = tur_loc, color = 'red', fill=NA, size = 0.3) +
  theme_void()

# plot D - cs - black
test4 <- ggplot() +
  labs(subtitle = "B1. Structural Constraints") +
  geom_sf(data = Lbdtre_wtr, color=NA, fill='black') +
  geom_sf(data = Lbdtre_rail, color=NA, fill='black') +
  geom_sf(data = Lbdtre_road, color=NA, fill='black') +
  geom_sf(data = tur_loc, color = 'red', fill=NA, size = 0.3) +
  theme_void() +
  annotation_scale(location = "bl",height = unit(0.1, "cm"))

# plot E - cscp - black
test5 <- ggplot() +
  labs(subtitle = "B2. Conservation Constraints") +
  geom_sf(data = Lppr_ca, color=NA, fill='black') +
  geom_sf(data = Lptc_unesco,color=NA, fill='black') +
  geom_sf(data = Lptc_urbg, color=NA, fill='black') +
  geom_sf(data = Lptc_fc, color=NA, fill='black') +
  geom_sf(data = tur_loc, color = 'red', fill=NA, size = 0.3) +
  theme_void()

# plot F - cscplup - black
test6 <- ggplot() +
  labs(subtitle = "B3. Land Use Constraints",
       caption = "SOURCE: PTC2,2015; PAI,2021; PRG,2021; BDTRE,2022") +
  geom_sf(data = Lpai_fz, color=NA, fill='black') +
  geom_sf(data = Lprg_png,color=NA, fill='black') +
  geom_sf(data = Lprg_cem, color=NA, fill='black') +
  geom_sf(data = Lprg_hlth, color=NA, fill='black') +
  geom_sf(data = Lprg_edu, color=NA, fill='black') +
  geom_sf(data = Lprg_ind, color=NA, fill='black') +
  geom_sf(data = tur_loc, color = 'red', fill=NA, size = 0.3) +
  theme_void() +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)

# test plot together
plot_c_constraints <- plot_grid(ncol = 3,
                                test1, test2, test3, test4, test5, test6,
                                align = "hv")
plot_c_constraints

# save plot
ggsave(filename = "plot_c_constraints.png",
       device = "png",
       path = here::here("figs/"),
       width = 9,
       height = 6,
       units = "in",
       dpi = 300)

# free memory
fm 

# PART 10 - LUTI & Selection Plots -----------------------------------

# plot luti results
# plot 1
test1 <- ggplot(zones_luti) +
  labs(title = "Population Increase by Functional Urban Area Zone",
       subtitle = "A. 2019-2030") +
  geom_sf(aes(fill = PopCh19_30pct),
          color = NA) +
  scale_fill_viridis_c(option = "B") +
  guides(fill=guide_legend(title="%")) +
  geom_sf(data = zones_luti, color = 'grey', fill=NA, size = 0.03) +
  theme_void() +
  annotation_scale(location = "bl",height = unit(0.1, "cm"))
test1

# plot 2
test2 <- ggplot(zones_luti) +
  labs(title = "",
       subtitle = "B. Zones Selected for Density Analysis",
       caption = "SOURCE: HARMONY LUTI MODEL, 2022") +
  geom_sf(data = zones_luti, color = 'black', fill=NA, size = 0.03) +
  geom_sf(data = zones_loc, color = 'red', fill=NA, size = 0.5) +
  theme_void() +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)
test2 

# Plot Together
plot_lutiresults <- plot_grid(ncol = 2,
                              test1, test2,
                              align = "h",
                              axis = "bt",
                              rel_widths = c(1.2, 1))
plot_lutiresults

# save plot
ggsave(filename = "plot_lutiresults.png",
       device = "png",
       path = here::here("figs/"),
       width = 7.8,
       height = 3.2,
       units = "in",
       dpi = 300)

# free memory
fm 

# PART 11 - Population Statistics -----------------------------------

# Create new dataframe for our area statistics
zone_popStats <- zones_luti

# remove uneeded information
zone_popStats <- dplyr::select(zone_popStats, -c(HAcar19, HAbus19, HArail19, 
                                                   HAcar30, HAbus30,  HArail30, 
                                                   HAC1930car, HAC1930bus, HAC1930rai,
                                                   JAcar19, JAbus19, JArail19, JAcar30, 
                                                   JAbus30, JArail30, JAC1930car, 
                                                   JAC1930bus, JAC1930rai, PopCh19_30pct,
                                                   PopCH19_30rl))

# rename columns
zone_popStats <- zone_popStats %>% 
  rename(Pop19 = OiPred_Tot,
         Pop30 = OiPred_T_1,
         Atot = area)

# calculate population density columns
zone_popStats <- zone_popStats %>%
  mutate(Popden19 = Atot/Pop19,
         Popden30 = Atot/Pop30)

# export to geopackage
st_write(obj = zone_popStats,
         here::here("clean_data/zone_popStats.gpkg"),
         delete_layer = TRUE)

# Note density zones:
# TL = NO == 670
# TR = NO == 668
# BL = NO == 1016
# BR = NO == 1002

# Example code for area of constraint layer in zones
# X <- constraint layer ex. Rppr_ca
# Y <- area of constraint layer ex. Appr_ca
#intersects <- st_intersection(zone_areaStats, X) %>% 
#  mutate(merge = paste0(NO, "-", Unnamed_.0)) %>% 
#  mutate(Y = st_area(.)) %>%
#  dplyr::select(., c(NO, Y))
#intersects

# free memory
fm 


# END OF PRE-PROCESSING SCRIPT --------------------------------------------------------


# See script 2_rasterize for next steps



# CODE CREDITS! --------------------------------------------------------

# ‘Add Common Legend to Combined ggplot2 R Plots | patchwork & gridExtra’. (no date). Statistics Globe. Available at: https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/ (Accessed: 5 September 2022).

# Aligning plots. (no date). Available at: https://wilkelab.org/cowplot/articles/aligning_plots.html (Accessed: 5 September 2022).

# Arrange multiple plots into a grid — plot_grid. (no date). Available at: https://wilkelab.org/cowplot/reference/plot_grid.html (Accessed: 5 September 2022).

# Arun. (2013). ‘Answer to “How to change legend title in ggplot”’. Stack Overflow. Available at: https://stackoverflow.com/a/14622513 (Accessed: 5 September 2022).

# as_units function - RDocumentation. (no date). Available at: https://www.rdocumentation.org/packages/units/versions/0.5-0/topics/as_units (Accessed: 5 September 2022).

# Choropleth maps in ggplot2. (2021). R CHARTS | A collection of charts and graphs made with the R programming language. Available at: https://r-charts.com/spatial/choropleth-map-ggplot2/ (Accessed: 5 September 2022).

# Clip raster to shapefile and merge raster layers in R - YouTube. (no date). Available at: https://www.youtube.com/watch?v=xbtyaja8tro&list=PL4aUQR9L9RFrP7tNSM3m8-xXfGKTbRIND&index=9 (Accessed: 5 September 2022).

# colourpicker - Google Search. (no date). Available at: https://www.google.com/search?q=colourpicker&rlz=1C1CHBF_enCA788CA788&oq=colourpicker&aqs=chrome..69i57j46i131i199i433i465i512j0i433i512j0i3j0i512l2j0i10i512j0i3j0i512.1433j0j1&sourceid=chrome&ie=UTF-8 (Accessed: 5 September 2022).

# Dorothy. (2018). ‘Answer to “how to efficiently import multiple raster (.tif) files into R”’. Stack Overflow. Available at: https://stackoverflow.com/a/52769247 (Accessed: 5 September 2022).

# Engel, C. A. (no date). Chapter 2 Spatial data manipulation in R | Using Spatial Data with R. Available at: https://cengel.github.io/R-spatial/spatialops.html (Accessed: 5 September 2022).

# GeoTiff Files in R: Import, Export, Visualize, Terrain Attributes. (2020). Available at: https://www.youtube.com/watch?v=kk-UZpewOys (Accessed: 5 September 2022).

# ggplot2 title : main, axis and legend titles - Easy Guides - Wiki - STHDA. (no date). Available at: http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles (Accessed: 5 September 2022).

# Guz. (2017). ‘Answer to “Set the minimum and maximum values for an elevation raster in R”’. Geographic Information Systems Stack Exchange. Available at: https://gis.stackexchange.com/a/241431 (Accessed: 5 September 2022).

# Hijmans, R. (2013). ‘Answer to “Merging multiple rasters in R”’. Stack Overflow. Available at: https://stackoverflow.com/a/16007522 (Accessed: 5 September 2022).

# Intro to Geospatial Data with R: Intro to Raster Data in R. (no date). Available at: https://erinbecker.github.io/r-raster-vector-geospatial/01-raster-structure/index.html (Accessed: 5 September 2022).

# Intro to Raster Data – Introduction to Geospatial Raster and Vector Data with R. (no date). Available at: https://datacarpentry.org/r-raster-vector-geospatial/01-raster-structure/index.html (Accessed: 5 September 2022).

# Mather, M. (no date). Section 7 Easier analysis with the tidyverse | Rad: R for academics. Available at: https://bookdown.org/marius_mather/Rad/ (Accessed: 5 September 2022).

# Mieno, T. (no date). 3.4 Spatial Intersection (cropping join) | R as GIS for Economists. Available at: https://tmieno2.github.io/R-as-GIS-for-Economists/spatial-intersection-cropping-join.html (Accessed: 5 September 2022).

# Modify axis, legend, and plot labels — labs. (no date). Available at: https://ggplot2.tidyverse.org/reference/labs.html (Accessed: 5 September 2022).

# Peek, R. (no date). Spatial Joins In R. Available at: https://ryanpeek.org/2019-04-29-spatial-joins-in-R/ (Accessed: 5 September 2022).

# Raster Data in R - The Basics | NSF NEON | Open Data to Understand our Ecosystems. (no date). Available at: https://www.neonscience.org/resources/learning-hub/tutorials/raster-data-r (Accessed: 5 September 2022).

# ‘Rename Data Frame Columns in R’. (no date). Datanovia. Available at: https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/ (Accessed: 5 September 2022).

# Reproject Raster Data – Introduction to Geospatial Raster and Vector Data with R. (no date). Available at: https://datacarpentry.org/r-raster-vector-geospatial/03-raster-reproject-in-r/index.html (Accessed: 5 September 2022).

# RPubs - Zonal statistics with polygons in R. (no date). Available at: https://rpubs.com/rural_gis/254726 (Accessed: 5 September 2022).

# Sacramento, R. (2019). ‘Answer to “Setting the units of each column of an existing dataframe in R”’. Stack Overflow. Available at: https://stackoverflow.com/a/57072488 (Accessed: 5 September 2022).

# Scriven, R. (2015). ‘Answer to “list.files() all files in directory and subdirectories”’. Stack Overflow. Available at: https://stackoverflow.com/a/32130788 (Accessed: 5 September 2022).

# SM, M. (2015). ‘Answer to “Extracting intersection areas in R”’. Geographic Information Systems Stack Exchange. Available at: https://gis.stackexchange.com/a/140536 (Accessed: 5 September 2022).

# Spatial modelling using ‘raster’ package - Part 1. (2018). Available at: https://www.youtube.com/watch?v=tm2dvPUKruo (Accessed: 5 September 2022).

# The magick package: Advanced Image-Processing in R. (no date). Available at: https://rstudio-pubs-static.s3.amazonaws.com/204991_da8f8dddaeab48b78735366f2c1c5475.html (Accessed: 5 September 2022).

# What is the rm() function in R? (no date). Educative: Interactive Courses for Software Developers. Available at: https://www.educative.io/answers/what-is-the-rm-function-in-r (Accessed: 5 September 2022).

