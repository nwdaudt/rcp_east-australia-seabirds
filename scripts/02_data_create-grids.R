##
## ASG - grids East Australia
## 
## *************************************************************************** #

## This code builds the East Australia grids on which data will be aggregated. 
## Grids are saved under "./data_out/grids_0.5-2.rda"

## Libraries ####

library(readr)
library(dplyr)
library(sf)
library(rnaturalearth)
# library(mapview) # only for visual checking

## Read and subset data ####

data <- readr::read_csv("./data_out/ASG_2016_2021_tidy.csv", 
                        show_col_types = FALSE)

## *********************** Get only 'East Australia' data ******************** #
east_aus <- dplyr::filter(data, longitude >= 141)

rm("data")

## Spatial objects ####

## Transform the 'df' into an 'sf' object
east_aus_sf <- 
  east_aus %>% 
  dplyr::mutate(lon = longitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview::mapview(east_aus_sf, zcol = "season")

## Get Australia 'sf'
aus_sf <- 
  rnaturalearth::ne_countries(scale = "medium", 
                              country = "australia", 
                              returnclass = "sf")

## Grids ####

grid_sizes <- c(0.5, 0.75, 1, 1.25, 1.5, 2)

grids <- list()

for (grid_size in grid_sizes) {
  
  name <- paste0("g", as.character(grid_size))
  
  g <- 
    sf::st_make_grid(east_aus_sf, 
                     cellsize = c(grid_size, grid_size), crs = 4326) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(IDgrid = 1:nrow(.)) %>%
    sf::st_difference(., aus_sf) %>%
    dplyr::select(IDgrid, x)
  
  grids[[name]] <- g
  
  rm("g", "name")
}

rm("grid_sizes", "grid_size")

# mapview::mapview(grids[[4]]) # just checking

## Save it ####

# save(grids, file = "./data_out/grids_0.5-2.rda")
