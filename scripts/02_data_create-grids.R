##
## ASG grids East Australia
## 
## Nicholas W Daudt
## ****************************************************************************#

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

## *********************** Get only 'East Australia' data *********************#
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
# [10] LC_TELEPHONE=C             LC_MEASUREMENT=en_NZ.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] rnaturalearth_0.1.0 sf_1.0-8            dplyr_1.0.9         readr_2.1.2        
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.9         rstudioapi_0.13    magrittr_2.0.3     units_0.8-0       
# [5] hms_1.1.1          tidyselect_1.1.2   lattice_0.20-45    R6_2.5.1          
# [9] rlang_1.0.4        fansi_1.0.3        tools_4.2.0        grid_4.2.0        
# [13] KernSmooth_2.23-20 utf8_1.2.2         e1071_1.7-11       cli_3.3.0         
# [17] DBI_1.1.3          class_7.3-20       ellipsis_0.3.2     assertthat_0.2.1  
# [21] tibble_3.1.7       lifecycle_1.0.1    crayon_1.5.1       purrr_0.3.4       
# [25] tzdb_0.3.0         vctrs_0.4.1        glue_1.6.2         sp_1.5-0          
# [29] proxy_0.4-27       compiler_4.2.0     pillar_1.7.0       generics_0.1.3    
# [33] classInt_0.4-7     pkgconfig_2.0.3
