##
## ASG environmental data extraction (for the full dataset)
## 
## Nicholas W Daudt
## ****************************************************************************#

## This code extracts all the environmental data that will be used in future analysis.
## Data sources are mentioned at the beginning of each section.
## As I've run this in my local computer, I did each variable individually, 
## due to memory issues. Also, due to same problem, I saved backups for each step 
## to avoid headaches.
## You can find all backups at "./data/backup-during-env-data-extraction/".
## In theory, this code doesn't need to be run again.

## Libraries ####

library(readr)
library(dplyr)
library(stringr)
library(sf)
library(mapview)
library(rnaturalearth)
library(raster)
library(terra)
library(rerddap)
library(rerddapXtracto)
# remotes::install_github("jebyrnes/hadsstR")
library(hadsstr)
library(tmap)

## Data ####

data <- readr::read_csv("./data_out/ASG_2016_2021_tidy.csv",
                        show_col_types = FALSE)

## Get parameters for downloading the data ####

data_summary <-
  data %>% 
  dplyr::group_by(voyage) %>%
  dplyr::summarise(date_start = min(date),
                   date_end = max(date),
                   lat_min = min(latitude),
                   lat_max = max(latitude),
                   lon_min = min(longitude),
                   lon_max = max(longitude),
                   n_records = n()) %>%
  dplyr::arrange(date_start)

# Note: I downloaded data manually

## Spatialize seabird data & get Australia polygon ####

data_sf <- 
  dplyr::mutate(data, lon = longitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview::mapview((data_sf %>% dplyr::filter(voyage == "fk201228")))

## AUSTRALIA polygon needs a bit of a hack to keep only 
## Australia's mainland and Tasmania
aus_sf <- 
  rnaturalearth::ne_countries(scale = "medium", 
                              country = "australia", 
                              returnclass = "sf")

aus_sf <- 
  sf::st_cast(aus_sf, 'MULTILINESTRING') %>% 
  sf::st_cast('LINESTRING', do_split = TRUE) %>%
  dplyr::mutate(npts = mapview::npts(geometry, by_feature = TRUE)) %>%
  sf::st_cast('POLYGON')

aus_sf <- 
  aus_sf %>% 
  dplyr::filter(npts == 1154 |    ## Continent
                npts == 162) %>%  ## Tasmania
  sf::st_union() %>%
  sf::st_sf()
# mapview::mapview(aus_sf)

## Bathymetry (bat), Slope (slope) & Distance from coast (dist_coast) ####

## Bathymetry data provided by GEBCO (2020)
## (https://www.gebco.net/data_and_products/gridded_bathymetry_data/)

r_bat <- 
  raster::raster("./data/env-data_raw/GEBCO_bathymetry/gebco_2020_n-8.5_s-49.0_w110.0_e160.0.nc")

# Project it
proj4string(r_bat) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
# raster::plot(r_bat)

r_slope <- raster::terrain(r_bat, opt = "slope")
# raster::plot(r_slope)

## Extract data 
data_sf_envs <- 
  data_sf %>% 
  dplyr::mutate(bat = raster::extract(r_bat, .),
                slope = round(raster::extract(r_slope, .), digits = 5),
                dist_coast = round(as.numeric(sf::st_distance(., aus_sf))/1000, digits = 1))

# plyr::count(is.na(data_sf_envs$bat))
# hist(data_sf_envs$bat)
# plyr::count(is.na(data_sf_envs$bat > 0))

## ----------------------------------------------------------------------------#
## Bathymetry (and slope) data consumed a lot of memory from my notebook, so   #
## create a backup here, and just read it later on. Additionally, get rid of   # 
## observations with 'dist_coast' equals to zero. They won't have              #
## 'oceanographic' data anyways in the following steps                         #
## ----------------------------------------------------------------------------#

readr::write_csv(
  (sf::st_drop_geometry(data_sf_envs) %>% dplyr::filter(! dist_coast == 0)), 
  "./data/backup-during-env-data-extraction/data_sf_physio-envs-backup.csv")

## Calculate EKE ####

# "Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – 
# IMOS is enabled by the National Collaborative Research Infrastructure strategy (NCRIS)."

## ----------------------------------------------------------------------------#
## V' and U' components were extracted and already saved (*_prime_stack.nc), 
## which were used to calculate EKE (eke_stack.nc).
## These next steps don't need to be run again, as it's all done and saved.
##
## However, to get the obj 'layer_names' which will be used in the next session, 
## you'll need to load "vcur_stack" (or "ucur_stack"; but note the code below used [v*])
## ----------------------------------------------------------------------------#

## V component ----------------------------------------------------------------#
vcur_stack <- raster::stack("./data/env-data_raw/IMOS_aggregation_20220919T105821Z.nc",
                            varname = "VCUR")
# raster::plot(vcur_stack)

## Find 'V prime'
v_clim <- raster::calc(vcur_stack,
                       function(x) raster::movingFun(x = x, n = 90, 
                                                fun = mean, na.rm = TRUE,
                                                type = 'to'))

v_prime_stack <- vcur_stack - v_clim
# raster::plot(v_prime_stack)

# Backup it
raster::writeRaster(v_prime_stack, 
                    file = "./data/env-data_raw/v_prime_stack.nc", 
                    format = "CDF")

## U component ----------------------------------------------------------------#
ucur_stack <- raster::stack("./data/env-data_raw/IMOS_aggregation_20220919T105821Z.nc",
                            varname = "UCUR")
# raster::plot(ucur_stack)

## Find 'U prime'
u_clim <- raster::calc(ucur_stack, 
                       function(x) raster::movingFun(x = x, n = 90,
                                                     fun = mean, na.rm = TRUE,
                                                     type = 'to'))

u_prime_stack <- ucur_stack - u_clim
# raster::plot(u_prime_stack)

# Backup it
raster::writeRaster(u_prime_stack, 
                    file = "./data/env-data_raw/u_prime_stack.nc", 
                    format = "CDF")


## ----------------------------------------------------------------------------#
## -----------------------LAYER NAMES (DATES)----------------------------------#

# layer_names <- names(vcur_stack) ## Some sort of 'Julian date'
# 
# # Find out the 'origin' date from these layers
# as.Date("2016-01-01") - 11322  # "2016-01-01" is the first day from the downloaded data
#                                # > origin = "1985-01-01"
# 
# # Check:
# # Number should get "2016-08-07", as I know "2016-08-08" doesn't have data and
# # can check layers names to verify if "X11542" exists [[it does not!]]
# as.Date(11541, origin = "1985-01-01") # OK!
#
# ## Create a vector to name the layers
# layer_names <-
#   paste0("X",
#          gsub(x = 
#                 as.character(
#                   as.Date(as.numeric(gsub(x = names(vcur_stack), pattern = "X", replacement = "")),
#                           origin = "1985-01-01")),
#               pattern = "-", replacement = "."))
## ----------------------------------------------------------------------------#

### Calculate EKE *********************[don't need to run again]************** #

## Load U' and V' data
v_prime_stack <- raster::stack("./data/env-data_raw/v_prime_stack.nc")
u_prime_stack <- raster::stack("./data/env-data_raw/u_prime_stack.nc")

## Calculate EKE
eke_stack <- ((u_prime_stack^2 + v_prime_stack^2) * 0.5)
# raster::plot(eke_stack) # Check -- OK

## Done! Save it
raster::writeRaster(eke_stack, 
                    file = "./data/env-data_raw/eke_stack.nc", 
                    format = "CDF",
                    overwrite = TRUE)
### ************************************************************************** #

## Extract EKE data (eke) ####

## -------------------------------------------------------------------------- ##
## Need to create the "layer_names" object to match data
## Check comments and code in the session above for more info... 
vcur_stack <- raster::stack("./data/env-data_raw/IMOS_aggregation_20220919T105821Z.nc",
                            varname = "VCUR")

## Create a vector to name the layers
layer_names <-
  paste0("X",
         gsub(x = 
                as.character(
                  as.Date(as.numeric(gsub(x = names(vcur_stack), pattern = "X", replacement = "")),
                          origin = "1985-01-01")),
              pattern = "-", replacement = "."))

rm("vcur_stack")
## -------------------------------------------------------------------------- ##

### Load backed-up seabird+env data -------------------------------------------#
data_sf_envs <- 
  readr::read_csv("./data/backup-during-env-data-extraction/data_sf_physio-envs-backup.csv", 
                  show_col_types = FALSE) %>%
  dplyr::mutate(lon = longitude, lat = latitude) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview::mapview(data_sf_envs)

## Check 'date' column
# class(data_sf_envs$date)
data_sf_envs$date <- as.Date(data_sf_envs$date)
# class(data_sf_envs$date) # Check - OK

### Load EKE data -------------------------------------------------------------#
eke_stack <- raster::stack("./data/env-data_raw/eke_stack.nc")

## Name the layers according to dates from data... (see above) ----------------#
names(eke_stack) <- layer_names
# raster::plot(eke_stack) # Check -- excellent

## FYI
# dates_with_no_data = 
#   c(as.Date(c("2016-08-08", "2016-08-30", "2016-09-22", "2018-02-05")),
#     seq.Date(from = as.Date("2016-10-15"), to = as.Date("2016-10-20"), by = "day"),
#     seq.Date(from = as.Date("2019-12-02"), to = as.Date("2019-12-31"), by = "day"),
#     seq.Date(from = as.Date("2020-12-02"), to = as.Date("2020-12-31"), by = "day"))

## Create a vector with the layer names transformed into class 'Date' ---------#
eke_dates <- as.Date(
  gsub(gsub(x = layer_names, pattern = "X", replacement = ""),
       pattern = "\\.", replacement = "-"))

## Extract data - matrix format -----------------------------------------------#
e_eke <- raster::extract(eke_stack, data_sf_envs)
# dim(e_eke) # Check - OK

## Get the right value - matching it up with 'eke_dates' vector ---------------#

# Create an empty vector that will store desired values
eke <- vector()

for(i in 1:nrow(e_eke)) {
  tryCatch({
    eke[i] <- e_eke[i,][which(eke_dates %in% data_sf_envs$date[i])]
  }, 
  error = function(e) {
    # If there is no match, 
    # or all values are empty (e.g. too close to the coastline):
    return(NA)
  })
}

## Add it to the main data table
data_sf_envs$eke <- round(eke, digits = 5)
# mapview::mapview(data_sf_envs, zcol = "eke")


## Backup data ----------------------------------------------------------------#
readr::write_csv(
  sf::st_drop_geometry(data_sf_envs), 
  "./data/backup-during-env-data-extraction/data_sf_physio-envs-eke-backup.csv")
# -----------------------------------------------------------------------------#


rm("vcur_stack", "eke_stack", "i", "layer_names", "eke_dates", "e_eke", "eke")

## Extract CHL data (chl & chl_log10) ####

### Load backed-up seabird+env data -------------------------------------------#
data_sf_envs <- 
  readr::read_csv("./data/backup-during-env-data-extraction/data_sf_physio-envs-eke-backup.csv", 
                  show_col_types = FALSE) %>%
  dplyr::mutate(lon = longitude, lat = latitude) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview::mapview(data_sf_envs)
### ---------------------------------------------------------------------------#

# Based on 
# (https://docs.ropensci.org/rerddap/articles/Using_rerddap.html)
# (https://coastwatch.pfeg.noaa.gov/projects/r/xyt-matchup.html) **

## Data from NOAA, Aqua-MODIS
## Data acquisition was made through the code below, 
## as such, I haven't downloaded any physical files.

## (https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMH1chla8day.html)
chlInfo <- info("erdMH1chla8day") 

# rerddap::cache_delete_all(force = TRUE) # to make sure {rerddap} cache is empty

xcoord <- data_sf_envs$longitude
ycoord <- data_sf_envs$latitude
tcoord <- data_sf_envs$date

## Extract data
ext_chl <- rerddapXtracto::rxtracto(chlInfo, 
                                  parameter = "chlorophyll", 
                                  xcoord = xcoord, 
                                  ycoord = ycoord, 
                                  tcoord = tcoord, 
                                  xlen = 0.2, ylen = 0.2)

data_sf_envs <- 
  dplyr::mutate(data_sf_envs,
                chl = ext_chl[["mean chlorophyll"]],
                chl_log10 = log10(ext_chl[["mean chlorophyll"]]))

# mapview::mapview(test, zcol = "chl")
# mapview::mapview(test, zcol = "chl_log")


## Backup data ----------------------------------------------------------------#
readr::write_csv(
  sf::st_drop_geometry(data_sf_envs), 
  "./data/backup-during-env-data-extraction/data_sf_physio-envs-eke-chl-backup.csv")
# -----------------------------------------------------------------------------#

rm("chlInfo", "ext_chl")

## Crop SST files ####

## "NOAA OI SST V2 High Resolution Dataset data provided by the NOAA PSL, Boulder, 
## Colorado, USA, from their website at https://psl.noaa.gov".
##             &
## Reynolds, Richard W., Thomas M. Smith, Chunying Liu, Dudley B. Chelton, 
## Kenneth S. Casey, Michael G. Schlax, 2007: Daily High-Resolution-Blended Analyses
## for Sea Surface Temperature. J. Climate, 20, 5473-5496

## Layers come as global maps, so we crop them here to include 
## only the Australian region

## SST data source (https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html)

## Create polygon box (around Australia) to crop files
study_area_polygon <- 
  sf::st_sfc(sf::st_polygon(list(rbind(c(110, -9),    # min(long) / min(lat)
                                       c(160, -9),    # max(long) / min(lat)
                                       c(160, -48),  # max(long) / max(lat) 
                                       c(110, -48),  # min(long) / max(lat)
                                       c(110, -9)))), # min(long) / min(lat)
             crs = 4326)
# mapview::mapview(study_area_polygon)

## Map SST netcdf files
file_dirs <- 
  list.files(normalizePath("./data/env-data_raw/"), full.names = TRUE)

file_dirs <- file_dirs[stringr::str_detect(file_dirs, "sst.day.mean")]
# file_dirs <- file_dirs[!stringr::str_detect(file_dirs, "_crop")]
# file_dirs <- file_dirs[!stringr::str_detect(file_dirs, "_SSTgrad")]

## Loop over the files and crop them, -----------------------------------------#
## otherwise it will be to heavy to run 'extract' [don't need to run again]

for (file_dir in file_dirs){
  
  # Read file
  r <- terra::rast(file_dir)
  
  # Extent to crop 
  ext <- terra::ext(as(study_area_polygon, "Spatial"))
  
  # Crop
  r_crop <- terra::crop(r, ext)
  # plot(r_crop)
  
  ## Save cropped file ********************************************************#
  terra::writeCDF(r_crop,
                  filename = paste0(stringr::str_sub(file_dir, end = -4), "_crop.nc"),
                  overwrite = TRUE)
  ## **************************************************************************#
  rm("r", "ext", "r_crop")
  gc()
}

rm("file_dirs", "file_dir", "study_area_polygon")

## Extract SST (sst) data ####

### Load backed-up seabird+env data -------------------------------------------#
data_sf_envs <- 
  readr::read_csv("./data/backup-during-env-data-extraction/data_sf_physio-envs-eke-chl-backup.csv", 
                  show_col_types = FALSE) %>%
  dplyr::mutate(lon = longitude, lat = latitude) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview::mapview(data_sf_envs)
### ---------------------------------------------------------------------------#

## Map SST netcdf *CROPED* files
file_dirs <- 
  list.files(normalizePath("./data/env-data_raw/"), full.names = TRUE)

file_dirs <- file_dirs[stringr::str_detect(file_dirs, "_crop")]

yrs <- c(2016:2021)
sst_v <- vector()

for (yr in yrs) {
  
  ## Subset seabird data by year
  sample <- dplyr::filter(data_sf_envs, year == yr)
  
  ## Read SST stack according to year sampled
  sst_stack <- 
    raster::stack(file_dirs[stringr::str_detect(file_dirs, as.character(yr))])
  
  ## Get layer names to match with seabird data by index
  layer_names <- names(sst_stack)
  layer_names <- as.Date(
    gsub(gsub(x = layer_names, pattern = "X", replacement = ""),
         pattern = "\\.", replacement = "-"))
  
  ## Extract data - matrix format
  e_sst <- raster::extract(sst_stack, sample)
  
  ## Get the right value - matching it up with 'layer_names'
  # Create an empty vector that will store desired values
  sst <- vector()
  for(i in 1:nrow(e_sst)) {
    sst[i] <- e_sst[i,][which(layer_names %in% sample$date[i])]
  }
  
  sst_v <- append(sst_v, sst)
  
  # Clean environment and garbage [memory]
  rm("sample", "sst_stack", "layer_names", "e_sst", "sst", "i")
  gc()
}

data_sf_envs <- dplyr::mutate(data_sf_envs, sst = sst_v)
# mapview::mapview(data_sf_envs, zcol = "sst")

## Backup data ----------------------------------------------------------------#
readr::write_csv(
  sf::st_drop_geometry(data_sf_envs), 
  "./data/backup-during-env-data-extraction/data_sf_physio-envs-eke-chl-sst-backup.csv")
# -----------------------------------------------------------------------------#

rm("yrs", "yr", "sst_v", "file_dirs")

## Extract SSS (sss) and MLD (mld) data ####

# Download from Copernicus
# For 2016-2019 data: GLOBAL_MULTIYEAR_PHY_001_030 (https://doi.org/10.48670/moi-00021)
# For 2020-2021 data: GLOBAL_ANALYSIS_FORECAST_PHY_001_024 (https://doi.org/10.48670/moi-00016)

### Load backed-up seabird+env data -------------------------------------------#
data_sf_envs <- 
  readr::read_csv("./data/backup-during-env-data-extraction/data_sf_physio-envs-eke-chl-sst-backup.csv", 
                  show_col_types = FALSE) %>%
  dplyr::mutate(lon = longitude, lat = latitude) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview::mapview(data_sf_envs)
### ---------------------------------------------------------------------------#

## Map SSS/MLD netcdf files
file_dirs <- 
  list.files(normalizePath("./data/env-data_raw/"), full.names = TRUE)

file_dirs <- file_dirs[stringr::str_detect(file_dirs, "phy")]

yrs <- c(2016:2021)
sss_v <- vector()
mld_v <- vector()

for (yr in yrs) {
  
  ## Subset seabird data by year
  sample <- dplyr::filter(data_sf_envs, year == yr)
  
  ## Read SSS/MLD stack according to year sampled
  sss_stack <- 
    raster::stack(file_dirs[stringr::str_detect(file_dirs, as.character(yr))],
                  varname = "so")
  mld_stack <- 
    raster::stack(file_dirs[stringr::str_detect(file_dirs, as.character(yr))],
                  varname = "mlotst")
  
  ## Get layer names to match with seabird data by index
  layer_names <- names(sss_stack)
  layer_names <- as.Date(
    gsub(gsub(x = layer_names, pattern = "X", replacement = ""),
         pattern = "\\.", replacement = "-"))
  
  ## Extract data - matrix format
  e_sss <- raster::extract(sss_stack, sample)
  e_mld <- raster::extract(mld_stack, sample)
  
  ## Get the right value - matching it up with 'layer_names'
  # Create empty vectors that will store desired values
  sss <- vector()
  mld <- vector()
  
  for(i in 1:nrow(e_sss)) {
    sss[i] <- e_sss[i,][which(layer_names %in% sample$date[i])]
  }
  
  for(i in 1:nrow(e_mld)) {
    mld[i] <- e_mld[i,][which(layer_names %in% sample$date[i])]
  }
  
  sss_v <- append(sss_v, sss)
  mld_v <- append(mld_v, mld)
  
  # Clean environment and garbage [memory]
  rm("sample", "layer_names", "i",
     "sss_stack", "e_sss", "sss", 
     "mld_stack", "e_mld", "mld")
  gc()
}

data_sf_envs <- dplyr::mutate(data_sf_envs, sss = sss_v)
data_sf_envs <- dplyr::mutate(data_sf_envs, mld = mld_v)
# mapview::mapview(data_sf_envs, zcol = "sss")
# mapview::mapview(data_sf_envs, zcol = "mld")

## Backup data ----------------------------------------------------------------#
readr::write_csv(
  sf::st_drop_geometry(data_sf_envs), 
  "./data/backup-during-env-data-extraction/data_sf_physio-envs-eke-chl-sst-sss-mld-backup.csv")
# -----------------------------------------------------------------------------#

rm("yrs", "yr", "sss_v", "mld_v", "file_dirs")

## Create SST gradient files ####

## Map SST netcdf *CROPED* files
file_dirs <- 
  list.files(normalizePath("./data/env-data_raw/"), full.names = TRUE)

file_dirs <- file_dirs[stringr::str_detect(file_dirs, "_crop")]

yrs <- c(2016:2021)

for (yr in yrs) {
  
  ## Read SST stack according to year sampled
  sst_stack <- 
    raster::stack(file_dirs[stringr::str_detect(file_dirs, as.character(yr))])
  # plot(sst_stack)
  
  r_Grad_stack <- raster::stack()
  
  for (i in 1:length(sst_stack@layers)) {
    
    r <- sst_stack[[i]]
    
    ## Get gradients ***********************************
    NS_Grad <- hadsstr::get_NS_diffs(r)
    WE_Grad <- hadsstr::get_WE_diffs(r)
    
    r_Grad <- hadsstr::get_spatial_gradient(NS_Grad, WE_Grad)
    r_Grad <- r_Grad$spatial_gradient
    # plot(r_Grad)
    
    r_Grad_stack <- raster::stack(r_Grad_stack, r_Grad)
    
    rm("r", "NS_Grad", "WE_Grad", "r_Grad", "i")
    gc()
  }
  
  # plot(r_Grad_stack)
  # length(r_Grad_stack@layers)
  
  ## Save cropped file ********************************************************#
  raster::writeRaster(r_Grad_stack,
                      filename = paste0(stringr::str_sub(
                        file_dirs[stringr::str_detect(file_dirs, as.character(yr))], end = -9), 
                        "_SSTgrad.nc"), 
                      format = "CDF", overwrite = TRUE)
  ## **************************************************************************#
  
  rm("sst_stack", "r_Grad_stack")
  gc()
}

rm("yrs", "yr", "file_dirs")

## Extract SSTgrad (sst_grad) data ####

### Load backed-up seabird+env data -------------------------------------------#
data_sf_envs <- 
  readr::read_csv("./data/backup-during-env-data-extraction/data_sf_physio-envs-eke-chl-sst-sss-mld-backup.csv", 
                  show_col_types = FALSE) %>%
  dplyr::mutate(lon = longitude, lat = latitude) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview::mapview(data_sf_envs)
### ---------------------------------------------------------------------------#

## Map SST netcdf *CROPED* files
file_dirs <- 
  list.files(normalizePath("./data/env-data_raw/"), full.names = TRUE)

file_dirs_sst <- file_dirs[stringr::str_detect(file_dirs, "_crop")]
file_dirs_sst_g <- file_dirs[stringr::str_detect(file_dirs, "_SSTgrad")]

yrs <- c(2016:2021)
sst_g_v <- vector()

for (yr in yrs) {
  
  ## Subset seabird data by year
  sample <- dplyr::filter(data_sf_envs, year == yr)
  
  ## Read SST stack according to year sampled
  sst_stack <- 
    raster::stack(file_dirs_sst[stringr::str_detect(file_dirs_sst, as.character(yr))])
  # plot(sst_stack)
  
  ## Read SST stack according to year sampled
  sst_g_stack <- 
    raster::stack(file_dirs_sst_g[stringr::str_detect(file_dirs_sst_g, as.character(yr))])
  # plot(sst_g_stack)
  
  ## Get layer names to match with seabird data by index
  layer_names <- names(sst_stack)
  layer_names <- as.Date(
    gsub(gsub(x = layer_names, pattern = "X", replacement = ""),
         pattern = "\\.", replacement = "-"))
  
  ## Extract data - matrix format
  e_sst_g <- raster::extract(sst_g_stack, sample)
  
  ## Get the right value - matching it up with 'layer_names'
  # Create an empty vector that will store desired values
  sst_g <- vector()
  for(i in 1:nrow(e_sst_g)) {
    sst_g[i] <- e_sst_g[i,][which(layer_names %in% sample$date[i])]
  }
  
  sst_g_v <- append(sst_g_v, sst_g)
  
  # Clean environment and garbage [memory]
  rm("sample", "sst_stack", "sst_g_stack", "layer_names", "e_sst_g", "sst_g", "i")
  gc()
}

data_sf_envs <- dplyr::mutate(data_sf_envs, sst_grad = sst_g_v)
# mapview::mapview(data_sf_envs, zcol = "sst_grad")

## Backup data ----------------------------------------------------------------#
readr::write_csv(
  sf::st_drop_geometry(data_sf_envs), 
  "./data/backup-during-env-data-extraction/data_sf_physio-envs-eke-chl-sst-sss-mld-sstgrad-backup.csv")
# -----------------------------------------------------------------------------#

rm("yrs", "yr", "sst_g_v", "file_dirs", "file_dirs_sst", "file_dirs_sst_g")

## Create climatology (static) SSTgrad layers [10yr & seasonal] ####

## SSTgrad from jan2011 to mar2021 --------------------------------------------#

## Map SST netcdf *CROPED* files
file_dirs <- 
  list.files(normalizePath("./data/env-data_raw/"), full.names = TRUE)

file_dirs <- file_dirs[stringr::str_detect(file_dirs, "_crop")]

## Stack all files
sst_stack_clim <- raster::stack()

for (i in 1:length(file_dirs)) {
  sst_stack_clim <- raster::stack(sst_stack_clim, 
                                  raster::stack(file_dirs[i]))
  rm("i")
}
# plot(sst_stack_clim)

sst_mean_clim <- raster::calc(sst_stack_clim, fun = mean)
# plot(sst_mean_clim)

## Get climatological gradients
NS_Grad <- hadsstr::get_NS_diffs(sst_mean_clim)
WE_Grad <- hadsstr::get_WE_diffs(sst_mean_clim)

r_Grad <- hadsstr::get_spatial_gradient(NS_Grad, WE_Grad)
r_Grad <- r_Grad$spatial_gradient
# plot(r_Grad)

## Back it up
raster::writeRaster(r_Grad,
                    filename = "data/env-data_raw/static_10yr_clim_SSTgrad.tif",
                    overwrite = TRUE)

rm("file_dirs", "sst_mean_clim", 
   "NS_Grad", "WE_Grad", "r_Grad")

## -------------- Seasonal SSTgrad --------------------------------------------#

## >>> get 'layer_names' object from the parent data
layer_names <- names(sst_stack_clim)

## Get a vector with only 'months' to match for sub-setting
nn <- as.integer(substr(layer_names, 7, 8))

sst_season_stack <- list(
  summer = raster::subset(sst_stack_clim, which(nn == 12 | nn == 1 | nn == 2)),
  autumn = raster::subset(sst_stack_clim, which(nn == 3 | nn == 4 | nn == 5)),
  winter = raster::subset(sst_stack_clim, which(nn == 6 | nn == 7 | nn == 8)),
  spring = raster::subset(sst_stack_clim, which(nn == 9 | nn == 10 | nn == 11))
)

seasons <- c("summer", "autumn", "winter", "spring")

for (season in seasons) {
  
  season_stack <- sst_season_stack[[season]]
  # raster::plot(season_stack)
  
  sst_mean_clim_season <- raster::calc(season_stack, fun = mean)
  # raster::plot(sst_mean_clim_season)
  
  ## Get gradients
  NS_Grad <- hadsstr::get_NS_diffs(sst_mean_clim_season)
  WE_Grad <- hadsstr::get_WE_diffs(sst_mean_clim_season)
  
  r_Grad <- hadsstr::get_spatial_gradient(NS_Grad, WE_Grad)
  r_Grad <- r_Grad$spatial_gradient
  # plot(r_Grad)
  
  ## Save it
  raster::writeRaster(r_Grad,
                      filename = 
                        paste0("data/env-data_raw/static_10yr_clim_SSTgrad_",
                        as.character(season), ".tif"),
                      overwrite = TRUE)
  
  rm("season_stack", "sst_mean_clim_season", 
     "NS_Grad", "WE_Grad", "r_Grad",
     "season")
  gc()
}

rm("sst_season_stack", "sst_stack_clim", "seasons", "layer_names", "nn")

## Create climatology (static) EKEmean & EKEsd layers [11yr & seasonal] ####

## EKE Mean and SD, mar2010--mar2021 ------------------------------------------------#

## V component ----------------------------------------------------------------#
vcur_stack <- raster::stack("./data/env-data_raw/IMOS_aggregation_20221010T031643Z.nc",
                            varname = "VCUR")
# raster::plot(vcur_stack)

## Create a vector to name the layers 
## Keep it for later processing (seasonal layers)
layer_names <-
  paste0("X",
         gsub(x = 
                as.character(
                  as.Date(as.numeric(gsub(x = names(vcur_stack), pattern = "X", replacement = "")),
                          origin = "1985-01-01")),
              pattern = "-", replacement = "."))

## Find 'V prime'
v_clim <- raster::calc(vcur_stack,
                       function(x) raster::movingFun(x = x, n = 90, 
                                                     fun = mean, na.rm = TRUE,
                                                     type = 'to'))

v_prime_stack <- vcur_stack - v_clim
# raster::plot(v_prime_stack)

# Backup it
raster::writeRaster(v_prime_stack, 
                    file = "./data/env-data_raw/v_prime_clim_stack.nc", 
                    format = "CDF")

rm("vcur_stack", "v_clim", "v_prime_stack")

## U component ----------------------------------------------------------------#
ucur_stack <- raster::stack("./data/env-data_raw/IMOS_aggregation_20221010T031643Z.nc",
                            varname = "UCUR")
# raster::plot(ucur_stack)

## Find 'U prime'
u_clim <- raster::calc(ucur_stack, 
                       function(x) raster::movingFun(x = x, n = 90,
                                                     fun = mean, na.rm = TRUE,
                                                     type = 'to'))

u_prime_stack <- ucur_stack - u_clim
# raster::plot(u_prime_stack)

# Backup it
raster::writeRaster(u_prime_stack, 
                    file = "./data/env-data_raw/u_prime_clim_stack.nc", 
                    format = "CDF")

rm("ucur_stack", "u_clim", "u_prime_stack")

## Calculate EKE
# (due to memory issues, I needed to restart the session)
# Needs to get the 'layer_names' obj above, though

## Load U' and V' data
v_prime_clim_stack <- raster::stack("./data/env-data_raw/v_prime_clim_stack.nc")
u_prime_clim_stack <- raster::stack("./data/env-data_raw/u_prime_clim_stack.nc")

## Calculate EKE
eke_stack <- ((u_prime_clim_stack^2 + v_prime_clim_stack^2) * 0.5)
# raster::plot(eke_stack) # Check -- OK

## Create climatology layers (mean & sd) --------------------------------------#
eke_mean_clim <- raster::calc(eke_stack, fun = mean)
# raster::plot(eke_mean_clim)

eke_sd_clim <- raster::calc(eke_stack, fun = sd)
# raster::plot(eke_sd_clim)

## Save it
raster::writeRaster(eke_mean_clim, 
                    file = "./data/env-data_raw/static_11yr_clim_EKEmean.tif",
                    overwrite = TRUE)

raster::writeRaster(eke_sd_clim, 
                    file = "./data/env-data_raw/static_11yr_clim_EKEsd.tif",
                    overwrite = TRUE)

rm("v_prime_clim_stack", "u_prime_clim_stack",
   "eke_mean_clim", "eke_sd_clim")

## ------------ Seasonal EKEmean & EKEsd --------------------------------------#

## >>> get 'layer_names' object from the parent data 'IMOS... .nc'
## >>> see code above (under 'V component')
# rm("vcur_stack")

## Get a vector with only 'months' to match for sub-setting
nn <- as.integer(substr(layer_names, 7, 8))

eke_season_stack <- list(
  summer = raster::subset(eke_stack, which(nn == 12 | nn == 1 | nn == 2)),
  autumn = raster::subset(eke_stack, which(nn == 3 | nn == 4 | nn == 5)),
  winter = raster::subset(eke_stack, which(nn == 6 | nn == 7 | nn == 8)),
  spring = raster::subset(eke_stack, which(nn == 9 | nn == 10 | nn == 11))
)

seasons <- c("summer", "autumn", "winter", "spring")

for (season in seasons) {
  
  season_stack <- eke_season_stack[[season]]
  # raster::plot(r_stack)
  
  eke_mean_clim_season <- raster::calc(season_stack, fun = mean)
  # raster::plot(eke_mean_clim_season)
  
  eke_sd_clim_season <- raster::calc(season_stack, fun = sd)
  # raster::plot(eke_sd_clim_season)
  
  ## Save it
  raster::writeRaster(eke_mean_clim_season, 
                      file = 
                        paste0("./data/env-data_raw/static_11yr_clim_EKEmean_", 
                                    as.character(season), ".tif"),
                      overwrite = TRUE)
  
  raster::writeRaster(eke_sd_clim_season, 
                      file = 
                        paste0("./data/env-data_raw/static_11yr_clim_EKEsd_", 
                               as.character(season), ".tif"),
                      overwrite = TRUE)
  
  rm("season_stack", "eke_mean_clim_season", "eke_sd_clim_season", "season")
  gc()
}

rm("eke_stack", "eke_season_stack", "seasons", "layer_names", "nn")

## EDA -- plot climatic data #### 

# tmaptools::palette_explorer()

## Map files
file_dirs <- 
  list.files(normalizePath("./data/env-data_raw/"), full.names = TRUE)

file_dirs <- file_dirs[stringr::str_detect(file_dirs, ".tif")]

seasons <- c("summer", "autumn", "winter", "spring")

# Only climatology files
file_dirs_clim <- file_dirs[(grepl(x = file_dirs, pattern = paste(seasons, collapse = "|"))) == FALSE]

## Plots ----------------------------------------------------------------------#

plot_list <- list()

## Seasonal plots

for (season in seasons) {
  
  file_dirs_season <- file_dirs[stringr::str_detect(file_dirs, 
                                                    paste0(as.character(season), ".tif"))]
  r_SSTgrad <- 
    raster::raster(file_dirs_season[stringr::str_detect(file_dirs_season, "SSTgrad")])
  r_EKEmean <- 
    raster::raster(file_dirs_season[stringr::str_detect(file_dirs_season, "EKEmean")])
  r_EKEsd <- 
    raster::raster(file_dirs_season[stringr::str_detect(file_dirs_season, "EKEsd")])
  
  ## Plots done using {tmap} tools
  nameSSTgrad <- paste("SST gradient", as.character(season))
  SSTgrad_plot <-
    tm_shape(r_SSTgrad) + tm_raster(palette = viridisLite::viridis(5), title = nameSSTgrad) + 
    tm_legend(legend.position = c(0.3, 0.45))
  
  nameEKEmean <- paste("EKE mean", as.character(season))
  EKEmean_plot <-
    tm_shape(r_EKEmean) + tm_raster(palette = viridisLite::magma(5), title = nameEKEmean) + 
    tm_legend(legend.position = c(0.3, 0.45))
  
  nameEKEsd <- paste("EKE sd", as.character(season))
  EKEsd_plot <-
    tm_shape(r_EKEsd) + tm_raster(palette = viridisLite::cividis(5), title = nameEKEsd) + 
    tm_legend(legend.position = c(0.3, 0.45))
  
  p <- tmap_arrange(SSTgrad_plot, EKEmean_plot, EKEsd_plot, 
                    nrow = 3)
  
  ## Store 'tmap_arrange' obj in a list
  name <- as.character(season)
  plot_list[[name]] <- p
  
  rm("r_SSTgrad", "r_EKEmean", "r_EKEsd",
     "SSTgrad_plot", "EKEmean_plot", "EKEsd_plot",
     "nameSSTgrad", "nameEKEmean", "nameEKEsd",
     "p", "name", "season", "file_dirs_season")
  gc()
}

## Climatology plots

for (file_dir_clim in file_dirs_clim) {
  
  r_SSTgrad <- 
    raster::raster(file_dirs_clim[stringr::str_detect(file_dirs_clim, "SSTgrad")])
  r_EKEmean <- 
    raster::raster(file_dirs_clim[stringr::str_detect(file_dirs_clim, "EKEmean")])
  r_EKEsd <- 
    raster::raster(file_dirs_clim[stringr::str_detect(file_dirs_clim, "EKEsd")])
  
  ## Plots done using {tmap} tools
  SSTgrad_plot <-
    tm_shape(r_SSTgrad) + tm_raster(palette = viridisLite::viridis(5), title = "SSTgrad 10 yrs") + 
    tm_legend(legend.position = c(0.3, 0.45))
  
  EKEmean_plot <-
    tm_shape(r_EKEmean) + tm_raster(palette = viridisLite::magma(5), title = "EKE mean 11 yrs") + 
    tm_legend(legend.position = c(0.3, 0.45))
  
  EKEsd_plot <-
    tm_shape(r_EKEsd) + tm_raster(palette = viridisLite::cividis(5), title = "EKE sd 11 yrs") +
    tm_legend(legend.position = c(0.3, 0.45))
  
  p <- tmap_arrange(SSTgrad_plot, EKEmean_plot, EKEsd_plot, 
                    nrow = 3)
  
  ## Store 'tmap_arrange' obj in a list
  name <- "climatology"
  plot_list[[name]] <- p
  
  rm("r_SSTgrad", "r_EKEmean", "r_EKEsd",
     "SSTgrad_plot", "EKEmean_plot", "EKEsd_plot",
     "p", "name", "file_dir_clim")
  gc()
}

climatic_layers_plot <- 
  tmap_arrange(c(plot_list[["summer"]],
                 plot_list[["autumn"]],
                 plot_list[["winter"]],
                 plot_list[["spring"]],
                 plot_list[["climatology"]]), ncol = 3)

tmap_save(tm = climatic_layers_plot,
          filename = "./figs/EDA_climatology_layers.pdf",
          width = 50, height = 55, units = "cm",
          dpi = 250)

rm("file_dirs", "file_dirs_clim", "seasons", 
   "plot_list", "climatic_layers_plot")

## Extract climatic data ####

### Load backed-up seabird+env data -------------------------------------------#
data_sf_envs <- 
  readr::read_csv("./data/backup-during-env-data-extraction/data_sf_physio-envs-eke-chl-sst-sss-mld-sstgrad-backup.csv", 
                  show_col_types = FALSE) %>%
  dplyr::mutate(lon = longitude, lat = latitude) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

## Create a continuous row ID
data_sf_envs <- dplyr::mutate(data_sf_envs, ID = 1:nrow(data_sf_envs), 
                              .before = everything())
### ---------------------------------------------------------------------------#

## Create vector to loop through and select file_dirs layers
seasons <- c("summer", "autumn", "winter", "spring")

## Map files
file_dirs <- 
  list.files(normalizePath("./data/env-data_raw/"), full.names = TRUE)

file_dirs <- file_dirs[Reduce("|", lapply(seasons, function(x) grepl(x, file_dirs)))]

data <- data.frame() ## To store data
for (i in 1:length(seasons)) {
  
  # Sample data according to season
  sample <- dplyr::filter(data_sf_envs, season == seasons[i])
  
  # Subset file names to read files according to season
  season_files <- file_dirs[stringr::str_detect(file_dirs, as.character(seasons[i]))]
  
  # Read raster layers
  r_sstgrad <- terra::rast(season_files[stringr::str_detect(season_files, "SSTgrad")])
  # plot(r_sstgrad)
  r_ekemean <- terra::rast(season_files[stringr::str_detect(season_files, "EKEmean")])
  # plot(r_ekemean)
  r_ekesd <- terra::rast(season_files[stringr::str_detect(season_files, "EKEsd")])
  # plot(r_ekesd)
  
  sample_df <- 
    sample %>%
    dplyr::mutate(clim_sst_grad = terra::extract(r_sstgrad, sample, ID = FALSE)[, 2],
                  clim_eke_mean = terra::extract(r_ekemean, sample, ID = FALSE)[, 2],
                  clim_eke_sd = terra::extract(r_ekesd, sample, ID = FALSE)[, 2]) %>%
    sf::st_drop_geometry()
  
  data <- rbind(data, sample_df)
  
  rm("i", "sample", "season_files", 
     "r_sstgrad", "r_ekemean", "r_ekesd",
     "sample_df")
}

## Backup data ----------------------------------------------------------------#
readr::write_csv(data, 
  "./data/backup-during-env-data-extraction/data_sf_physio-envs-eke-chl-sst-sss-mld-sstgrad-climaticlayers-backup.csv")

# Save it under 'data_out' as well
readr::write_csv(data,
  "./data_out/ASG_2016_2021_tidy-with-env-vars.csv")
# -----------------------------------------------------------------------------#

rm("data", "data_sf_envs", "file_dirs", "seasons")

#### `sessionInfo()` ####

# sessionInfo()

# R version 4.2.0 (2022-04-22)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 20.04.5 LTS
# 
# Matrix products: default
# BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
# LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
# 
# locale:
# [1] LC_CTYPE=en_NZ.UTF-8       LC_NUMERIC=C               LC_TIME=en_NZ.UTF-8       
# [4] LC_COLLATE=en_NZ.UTF-8     LC_MONETARY=en_NZ.UTF-8    LC_MESSAGES=en_NZ.UTF-8   
# [7] LC_PAPER=en_NZ.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
# [10] LC_TELEPHONE=C            LC_MEASUREMENT=en_NZ.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] hadsstr_0.1          rerddapXtracto_1.1.2 rerddap_0.8.0        terra_1.6-7         
# [5] raster_3.5-21        sp_1.5-0             rnaturalearth_0.1.0  mapview_2.11.0      
# [9] sf_1.0-8             stringr_1.4.0        dplyr_1.0.9          readr_2.1.2         
# 
# loaded via a namespace (and not attached):
# [1] bit64_4.0.5             vroom_1.5.7             jsonlite_1.8.0          assertthat_0.2.1       
# [5] stats4_4.2.0            yaml_2.3.5              pillar_1.7.0            lattice_0.20-45        
# [9] glue_1.6.2              uuid_1.1-0              digest_0.6.29           colorspace_2.0-3       
# [13] leaflet.providers_1.9.0 htmltools_0.5.2         pkgconfig_2.0.3         httpcode_0.3.0         
# [17] purrr_0.3.4             scales_1.2.0            webshot_0.5.3           brew_1.0-7             
# [21] svglite_2.1.0           satellite_1.0.4         tzdb_0.3.0              tibble_3.1.7           
# [25] proxy_0.4-27            farver_2.1.1            generics_0.1.3          tictoc_1.1             
# [29] ellipsis_0.3.2          withr_2.5.0             cli_3.3.0               magrittr_2.0.3         
# [33] crayon_1.5.1            ncdf4_1.19              fansi_1.0.3             xml2_1.3.3             
# [37] class_7.3-20            tools_4.2.0             data.table_1.14.2       hms_1.1.1              
# [41] lifecycle_1.0.1         munsell_0.5.0           compiler_4.2.0          e1071_1.7-11           
# [45] systemfonts_1.0.4       rlang_1.0.4             classInt_0.4-7          units_0.8-0            
# [49] grid_4.2.0              leafpop_0.1.0           rstudioapi_0.13         rappdirs_0.3.3         
# [53] htmlwidgets_1.5.4       crosstalk_1.2.0         leafem_0.2.0            base64enc_0.1-3        
# [57] codetools_0.2-18        DBI_1.1.3               curl_4.3.2              R6_2.5.1               
# [61] rgdal_1.5-32            fastmap_1.1.0           bit_4.0.4               utf8_1.2.2             
# [65] KernSmooth_2.23-20      hoardr_0.5.2            stringi_1.7.6           parallel_4.2.0         
# [69] crul_1.3                Rcpp_1.0.9              vctrs_0.4.1             png_0.1-7              
# [73] leaflet_2.1.1           tidyselect_1.1.2 
