##
## ASG -- another RCP trial (NZMSS)
## Data wrangling
## 
## Nicholas W Daudt
##

## Libraries ####
library(plyr)
library(tidyverse)
library(lubridate)
library(sf)

## Data ####

data <- readr::read_csv("./data_out/ASG_2016_2021_tidy.csv", 
                        show_col_types = FALSE)

###
### SCALED ENVIRONMENTAL DATA - 

###


## Only birds identified to species level
data_sp <- dplyr::filter(data, !is.na(species))

## Get only 'East Australia' data *********************************************# **
east_aus <- dplyr::filter(data_sp, longitude >= 141)

rm("data", "data_sp")

### summary of data ------------------------------------------#
# length(unique(east_aus$voyage))   # > 14 voyages
# 
# min(east_aus$date); max(east_aus$date)
#                                   # 2016-08-25; 2021-01-25
# 
# nrow(east_aus); plyr::count(east_aus$season)
#                                   # > 9927 observations/records, in all seasons
#                                   # > autumn 2107
#                                   # > spring 3533
#                                   # > summer 2248
#                                   # > winter 2039
# 
# length(unique(east_aus$species))  # > 80 species
# length(unique(east_aus$genus))    # > 35 genus
# length(unique(east_aus$family))   # > 11 families
# length(unique(east_aus$order))    # > 5 orders
# 
# sum(east_aus$total_ct)            # > In total, 142,919 individuals recorded
### ---------------------------------------------------------#

## Spatial wrangling ####

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

## Create grids ***************************************************************#
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
#******************************************************************************#

## Spatial join seabird data and grids ----------------------------------------#

grids_east_aus <- lapply(grids, function(x) sf::st_join(east_aus_sf, x))

## Summarise data  -------------------------------------------------------------#

df_summaries <- data.frame()

for (i in 1:length(grids_east_aus)) {
  
  df <- 
    as.data.frame(grids_east_aus[[i]]) %>%
    dplyr::group_by(IDgrid, season) %>%
    dplyr::summarise(n_birds = sum(total_ct),
                     n_spp = n_distinct(species)) %>%
    dplyr::mutate(grid = names(grids_east_aus[i]))
  
  df_summaries <- rbind(df_summaries, df)
  
  rm("df")
}

df_summaries %>% dplyr::group_by(grid, season) %>% dplyr::summarise(n_distinct(IDgrid))

# gr_lab <- ggplot2::as_labeller(c(
#   `g0.5`  = "g0.5 (n = 335)",
#   `g0.75` = "g0.75 (n = 231)",
#   `g1`    = "g1 (n = 167)",
#   `g1.25` = "g1.25 (n = 140)",
#   `g1.5`  = "g1.5 (n = 109)",
#   `g2`    = "g2 (n = 78)"
# ))
# 
# plot_sp_richness_grids <-
#   ggplot(df_summaries, aes(x = n_spp)) +
#   geom_histogram(color = "black", fill = "white", binwidth = 1) +
#   facet_wrap(~ grid, labeller = gr_lab) +
#   ylab("Count") + xlab("Number of species per grid")+
#   theme_bw() + 
#   theme(strip.text.x = element_text(size = 12),
#         axis.text = element_text(size = 12),
#         axis.title = element_text(size = 12))
# 
# ggsave(plot_sp_richness_grids,
#        filename = "./figs/EDA_aggr_hist_0.5-2.pdf",
#        height = 20, width = 22, units = "cm", dpi = 200)
# 
# rm("gr_lab")

## SAVE gridded data ********************************************************###

save(grids_east_aus, file = "./data_out/grids_east_aus.rda")
# sf::st_write(grids_east_aus[["g1"]], "./data_out/grids_east_aus_g1.gpkg")
readr::write_csv(sf::st_drop_geometry(grids_east_aus[["g1"]]), "./data_out/grids_east_aus_g1.csv")
