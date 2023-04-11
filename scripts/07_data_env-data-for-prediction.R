##
## ASG environmental average layers and create prediction data.frame
## 
## Nicholas W Daudt
## ****************************************************************************#

# This code creates average ('climatic'), static layers for all environmental 
# data by season and, finally, average values by grid (1 x 1 degree) to be 
# used as basis for predicting results.

## These average layers were done during script '03_1_data_...':
# clim_EKEsd, 
# clim_EKEmean (consequently, eke), 
# clim_SSTgrad (consequently, sst_grad)

## And these are static by nature, so they are processed in the last section:
# bat, slope, dist_coast

## Libraries ####

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(sf)
library(sp)
library(mapview)
library(rnaturalearth)
library(raster)
library(terra)

## Create SST average layers ####

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
  
  ## Save it
  raster::writeRaster(sst_mean_clim_season,
                      filename = 
                        paste0("data/env-data_raw/static_10yr_clim_SST_",
                               as.character(season), ".tif"),
                      overwrite = TRUE)
  
  rm("season_stack", "sst_mean_clim_season",
     "season")
  gc()
}

rm("sst_season_stack", "sst_season_stack", 
   "sst_stack_clim", "seasons", "layer_names", "nn", "file_dirs")

## Create SSS & MLD average layers ####

# Download from Copernicus
# For 2011-2015 data: GLOBAL_MULTIYEAR_PHY_001_030 (https://doi.org/10.48670/moi-00021)
# >> As for this layer we don't need daily data, I downloaded *monthly* data
# >> to save storage and time.

## Map SSS/MLD netcdf files
file_dirs <- 
  list.files(normalizePath("./data/env-data_raw/"), full.names = TRUE)

file_dirs <- file_dirs[stringr::str_detect(file_dirs, "phy")]

## Read and stack all files
stack_sss <- raster::stack()
stack_mld <- raster::stack()

for (i in 1:length(file_dirs)) {
  stack_sss <- raster::stack(stack_sss,
                             raster::stack(file_dirs[i],
                                           varname = "so"))
  stack_mld <- raster::stack(stack_mld,
                             raster::stack(file_dirs[i],
                                           varname = "mlotst"))
  
  rm("i")
}
# plot(stack_sss)
# plot(stack_mld)

## >>> get 'layer_names' object from the parent data
layer_names <- names(stack_sss)

## Get a vector with only 'months' to match for sub-setting
nn <- as.integer(substr(layer_names, 7, 8))

seasons_stack_sss <- list(
  summer = raster::subset(stack_sss, which(nn == 12 | nn == 1 | nn == 2)),
  autumn = raster::subset(stack_sss, which(nn == 3 | nn == 4 | nn == 5)),
  winter = raster::subset(stack_sss, which(nn == 6 | nn == 7 | nn == 8)),
  spring = raster::subset(stack_sss, which(nn == 9 | nn == 10 | nn == 11))
)

seasons_stack_mld <- list(
  summer = raster::subset(stack_mld, which(nn == 12 | nn == 1 | nn == 2)),
  autumn = raster::subset(stack_mld, which(nn == 3 | nn == 4 | nn == 5)),
  winter = raster::subset(stack_mld, which(nn == 6 | nn == 7 | nn == 8)),
  spring = raster::subset(stack_mld, which(nn == 9 | nn == 10 | nn == 11))
)

seasons <- c("summer", "autumn", "winter", "spring")

for (season in seasons) {
  
  season_sss <- seasons_stack_sss[[season]]
  # raster::plot(season_sss)
  
  season_mld <- seasons_stack_mld[[season]]
  # raster::plot(season_mld)
  
  sss_mean_clim_season <- raster::calc(season_sss, fun = mean)
  # raster::plot(sss_mean_clim_season)
  
  mld_mean_clim_season <- raster::calc(season_mld, fun = mean)
  # raster::plot(mld_mean_clim_season)
  
  ## Save it
  raster::writeRaster(sss_mean_clim_season,
                      filename = 
                        paste0("data/env-data_raw/static_11yr_clim_SSS_",
                               as.character(season), ".tif"),
                      overwrite = TRUE)
  
  raster::writeRaster(mld_mean_clim_season,
                      filename = 
                        paste0("data/env-data_raw/static_11yr_clim_MLD_",
                               as.character(season), ".tif"),
                      overwrite = TRUE)
  
  rm("season_sss", "season_mld", 
     "sss_mean_clim_season", "mld_mean_clim_season",
     "season")
  gc()
}

rm("stack_sss", "stack_mld", "seasons_stack_sss", "seasons_stack_mld", 
   "seasons", "layer_names", "nn", "file_dirs")

## Create CHL average layers ####

## Data from NOAA, Aqua-MODIS
## (https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMH1chla8day.html)
# Due to the size of the files, I've downloaded 3 files 
# (2011-2014, 2015-2018, 2019-2021)

## Map CHL netcdf files
file_dirs <- 
  list.files(normalizePath("./data/env-data_raw/"), full.names = TRUE)

file_dirs <- file_dirs[stringr::str_detect(file_dirs, "erdMH1chla8day")]

## Read and stack all files
stack_chl <- raster::stack()

for (i in 1:length(file_dirs)) {
  stack_chl <- raster::stack(stack_chl,
                             raster::stack(file_dirs[i],
                                           varname = "chlorophyll"))
  rm("i")
}
# plot(stack_chl)

## >>> get 'layer_names' object from the parent data
layer_names <- names(stack_chl)

## Get a vector with only 'months' to match for sub-setting
nn <- as.integer(substr(layer_names, 7, 8))

seasons_stack_chl <- list(
  summer = raster::subset(stack_chl, which(nn == 12 | nn == 1 | nn == 2)),
  autumn = raster::subset(stack_chl, which(nn == 3 | nn == 4 | nn == 5)),
  winter = raster::subset(stack_chl, which(nn == 6 | nn == 7 | nn == 8)),
  spring = raster::subset(stack_chl, which(nn == 9 | nn == 10 | nn == 11))
)

seasons <- c("summer", "autumn", "winter", "spring")

for (season in seasons) {
  
  season_chl <- seasons_stack_chl[[season]]
  # raster::plot(season_chl)
  
  chl_mean_clim_season <- raster::calc(season_chl, fun = mean, na.rm = TRUE)
  # raster::plot(chl_mean_clim_season)
  
  ## Save it
  raster::writeRaster(chl_mean_clim_season,
                      filename = 
                        paste0("data/env-data_raw/static_11yr_clim_CHL_",
                               as.character(season), ".tif"),
                      overwrite = TRUE)
  
  rm("season_chl", "chl_mean_clim_season",
     "season")
  gc()
}

rm("stack_chl", "seasons_stack_chl", 
   "seasons", "layer_names", "nn", "file_dirs")

## Crop & extract data for each season ####

## Seasons vector
seasons <- c("summer", "autumn", "winter", "spring")

## Get the polygon extent from 'g1' to crop layers
load("./data_out/grids_0.5-2.rda")
g1 <- grids[["g1"]]
rm("grids")

g1_ext <- terra::ext(as(sf::st_as_sfc(sf::st_bbox(g1)), "Spatial"))

## Get Australia polygon (mainland + Tasmania)
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

aus_sf <- 
  sf::st_crop(aus_sf, sf::st_bbox(g1))
# mapview::mapview(aus_sf)

## Read static layers once, as they don't differ between seasons
## Bathymetry
r_bat <- terra::crop(
    terra::rast("./data/env-data_raw/GEBCO_bathymetry/gebco_2020_n-8.5_s-49.0_w110.0_e160.0.nc"),
    g1_ext)

r_bat <- terra::mask(r_bat, terra::vect(aus_sf), inverse = TRUE)
# terra::plot(r_bat)

## Slope
r_slope <- terra::terrain(r_bat, v = "slope")
# terra::plot(r_slope)

gc()

## Grid (g1) centroids
centroids_sf <- 
  g1 %>%
  sf::st_centroid(.)

centroids_df <- 
  g1 %>%
  sf::st_centroid(.) %>%
  dplyr::mutate(lon = unlist(purrr::map(.$x, 1)),
                lat = unlist(purrr::map(.$x, 2))) %>% 
  sf::st_drop_geometry() %>%
  as.data.frame()

## Load scaling parameters for environmental data 
# Note: this file comes from script "06"
load("./data_out/attributes-scaled-env-data-season.rda")

## Map seasonal static layers
file_dirs <- 
  list.files(normalizePath("./data/env-data_raw/"), full.names = TRUE)

file_dirs <- file_dirs[stringr::str_detect(file_dirs, "static_")]

# Remove files clim_EKEsd, clim_EKEmean, clim_SSTgrad (10/11yrs avg layers)
file_dirs <- file_dirs[grepl(x = file_dirs, 
                              pattern = paste(seasons, collapse = "|"))]

## pseudo-code ---------------------------------------#
# select one season
# open file & # crop to 'g1' extent
# extract mean value for each grid (remember: clim_EKEmean == eke, clim_SSTgrad == SSTgrad)
# Scale environmental data
# store as data.frame
## ---------------------------------------------------#

## Obj to store data
pred_data_seasons <- list()

for (season in seasons) {
  
  # Select only files from a particular season
  file_dirs_season <- file_dirs[stringr::str_detect(file_dirs, as.character(season))]
  
  # Open & crop each file as a separate rasters
  r_sst <- terra::crop(
    terra::rast(file_dirs_season[stringr::str_detect(file_dirs_season, "SST_")]),
    g1_ext)
  r_sst_grad <- terra::crop(
    terra::rast(file_dirs_season[stringr::str_detect(file_dirs_season, "SSTgrad")]),
    g1_ext)
  r_chl <- terra::crop(
    terra::rast(file_dirs_season[stringr::str_detect(file_dirs_season, "CHL")]),
    g1_ext)
  r_ekemean <- terra::crop(
    terra::rast(file_dirs_season[stringr::str_detect(file_dirs_season, "EKEmean")]),
    g1_ext)
  r_ekesd <- terra::crop(
    terra::rast(file_dirs_season[stringr::str_detect(file_dirs_season, "EKEsd")]),
    g1_ext)
  r_mld <- terra::crop(
    terra::rast(file_dirs_season[stringr::str_detect(file_dirs_season, "MLD")]),
    g1_ext)
  r_sss <- terra::crop(
    terra::rast(file_dirs_season[stringr::str_detect(file_dirs_season, "SSS")]),
    g1_ext)
  
  ## Extract data
  env_season <- 
    g1 %>%
    dplyr::mutate(bat_mean = terra::extract(r_bat, g1, fun = mean, na.rm = TRUE, ID = FALSE)[, 2],
                  slope_mean = round(terra::extract(r_slope, g1, fun = mean, na.rm = TRUE, ID = FALSE)[, 2], digits = 5),
                  dist_coast_mean = round(as.numeric(sf::st_distance(centroids_sf, aus_sf))/1000, digits = 1),
                  eke_mean = terra::extract(r_ekemean, g1, fun = mean, na.rm = TRUE, ID = FALSE)[, 2],
                  chl_mean = terra::extract(r_chl, g1, fun = mean, na.rm = TRUE, ID = FALSE)[, 2],
                  chl_log10_mean = log10(terra::extract(r_chl, g1, fun = mean, na.rm = TRUE, ID = FALSE)[, 2]),
                  sst_mean = terra::extract(r_sst, g1, fun = mean, na.rm = TRUE, ID = FALSE)[, 2],
                  sss_mean = terra::extract(r_sss, g1, fun = mean, na.rm = TRUE, ID = FALSE)[, 2],
                  mld_mean = terra::extract(r_mld, g1, fun = mean, na.rm = TRUE, ID = FALSE)[, 2],
                  sst_grad_mean = terra::extract(r_sst_grad, g1, fun = mean, na.rm = TRUE, ID = FALSE)[, 2],
                  clim_sst_grad_mean = terra::extract(r_sst_grad, g1, fun = mean, na.rm = TRUE, ID = FALSE)[, 2],
                  clim_eke_mean_mean = terra::extract(r_ekemean, g1, fun = mean, na.rm = TRUE, ID = FALSE)[, 2],
                  clim_eke_sd_mean = terra::extract(r_ekesd, g1, fun = mean, na.rm = TRUE, ID = FALSE)[, 2]) %>%
    sf::st_drop_geometry()
  
  gc() # A bit heavy for my PC -- so give it a slack, hehe
  
  ## Scale environmental data according to season-specific scaling coefficients
  scale_params <- 
    env_data_seasons[stringr::str_detect(names(env_data_seasons), as.character(season))]
  
  scale_params <- scale_params[[1]]
  
  pred_data_scaled <- 
    scale(env_season[, -1], 
          attr(scale_params, "scaled:center"), 
          attr(scale_params, "scaled:scale")) %>% 
    as.data.frame()
  
  pred_data_scaled <- 
    cbind(centroids_df, pred_data_scaled)
  
  ## Name the final data & store it into 'pred_data_seasons'
  name <- paste0("pred_data_", as.character(season))
  pred_data_seasons[[name]] <- pred_data_scaled
  
  rm("season", "file_dirs_season",
     "r_sst", "r_sst_grad", "r_chl", "r_ekemean", "r_ekesd", "r_mld", "r_sss",
     "env_season", "scale_params", "pred_data_scaled", "name")
  gc()
}

save(pred_data_seasons, file = "./data_out/pred-data-seasons.rda")

rm("r_bat", "r_slope", "env_data_seasons",
   "aus_sf", "centroids_sf", "centroids_df",
   "g1", "g1_ext", "file_dirs", "seasons")
gc()
