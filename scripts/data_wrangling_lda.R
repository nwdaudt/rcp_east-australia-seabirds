##
## ASG data wrangling
## 
## Nicholas W Daudt
## ****************************************************************************#

## Libraries ####

library(tidyverse)
library(lubridate)
library(sf)
library(rnaturalearth)
library(tmap)

## Read and subset data ####

data <- readr::read_csv("./data_out/ASG_2016_2021_tidy.csv", show_col_types = FALSE)

## Only birds identified to species level
data_sp <- dplyr::filter(data, !is.na(species))

## Get only 'East Australia' data *********************************************# **
east_aus <- dplyr::filter(data_sp, longitude >= 141)

## Get only 'spring' data *****************************************************# **
spring <- 
  east_aus %>%
  dplyr::filter(season == "spring") %>%
  droplevels(.)

## Spring summary -------------------------------------------------------------# **
spring_summary <-
  spring %>%
  dplyr::group_by(voyage) %>%
  dplyr::summarise(start_date = min(date),
                   end_date = max(date),
                   days_voyage = difftime(max(date), min(date), units = "days"),
                   lat_range = paste(max(latitude), ",", min(latitude)),
                   lon_range = paste(min(longitude), ",", max(longitude)),
                   sp_richness = n_distinct(as.factor(species)),
                   total_abund = sum(total_ct))

rm("data", "east_aus", "spring_summary")

## Spatial wrangling & EDA ####

## Transform the 'df' into an 'sf' object
spring_sf <- 
  spring %>% 
  dplyr::mutate(lon = longitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview::mapview(spring_sf, zcol = "voyage")

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
    sf::st_make_grid(spring_sf, 
                     cellsize = c(grid_size, grid_size), crs = 4326) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(IDgrid = 1:nrow(.)) %>%
    sf::st_difference(., aus_sf) %>%
    dplyr::select(IDgrid, x)
  
  grids[[name]] <- g
  
  rm("g", "name")
}

rm("grid_sizes", "grid_size")

## Spatial join seabird data and grids ****************************************#

grids_spring <- lapply(grids, function(x) sf::st_join(spring_sf, x))

## Summarise data

df_summaries <- data.frame()

for (i in 1:length(grids_spring)) {
  
  df <- 
    as.data.frame(grids_spring[[i]]) %>%
    dplyr::group_by(IDgrid) %>%
    dplyr::summarise(n_birds = sum(total_ct),
                     n_spp = n_distinct(species)) %>%
    dplyr::mutate(grid = names(grids_spring[i]))
  
  df_summaries <- rbind(df_summaries, df)
  
  rm("df")
}

plyr::count(df_summaries$grid)

gr_lab <- ggplot2::as_labeller(c(
  `g0.5`  = "g0.5 (n = 161)",
  `g0.75` = "g0.75 (n = 117)",
  `g1`    = "g1 (n = 93)",
  `g1.25` = "g1.25 (n = 82)",
  `g1.5`  = "g1.5 (n = 67)",
  `g2`    = "g2 (n = 49)"
))

plot_sp_richness_grids <-
  ggplot(df_summaries, aes(x = n_spp)) +
  geom_histogram(color = "black", fill = "white", binwidth = 1) +
  facet_wrap(~ grid, labeller = gr_lab) +
  ylab("Count") + xlab("Number of species per grid")+
  theme_bw() + 
  theme(strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

ggsave(plot_sp_richness_grids,
       filename = "./figs/EDA_aggr_hist_0.5-2.pdf",
       height = 20, width = 22, units = "cm", dpi = 200)

rm("gr_lab")

## SAVE FOR LDA TRIAL
save(grids_spring, file = "./data_out/grids_spring.rda")
# sf::st_write(grids_spring[["g1"]], "./data_out/grids_spring_g1.gpkg")
readr::write_csv(sf::st_drop_geometry(grids_spring[["g1"]]), "./data_out/grids_spring_g1.csv")

## Map grids/voyages **********************************************************#
# 
# map_g2 <-                              # 2 x 2 ********************************#
#   # Australia
#   tm_shape(aus_sf, bbox = sf::st_bbox(grid2)) + tm_polygons() + 
#   # Grid 2x2
#   tm_shape(grid2) + tm_borders() + 
#   # Seabird records, coloured by voyage
#   tm_shape(spring_g2) + tm_dots(col = "voyage", alpha = 0.5, size = .25) + 
#   # Aesthetics of the map
#   tm_grid(lines = FALSE) + tm_compass() +
#   tm_legend(legend.position = c(0.1, 0.4),
#             legend.bg.color = "white")
#   
# tmap_save(map_g2,
#           filename = "./figs/EDA_aggregation_map2.png",
#           width = 15, height = 20, units = "cm",
#           dpi = 250)
