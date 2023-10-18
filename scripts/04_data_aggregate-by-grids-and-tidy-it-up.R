##
## ASG - aggregate East Australia data by grids
## 
## *************************************************************************** #

## This code aggregate raw data by grid sizes/season,
## summarises environmental and seabird data by grid/season,
## and builds the final, wide-format dataset which will be used for modelling.

## Libraries ####

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(sf)
# library(mapview) # only used for visualization/checking purposes

## Data - seabirds ####

df <- readr::read_csv("./data_out/ASG_2016_2021_tidy-with-env-vars.csv", 
                      show_col_types = FALSE)

## Subset data off East Australia
df_EAus <- dplyr::filter(df, longitude >= 141)

## Keep only records identified to species level
df_EAus <- dplyr::filter(df_EAus, !is.na(species)) 

## Remove ship-followers from the dataset
df_id_w_only_followers <-
  df_EAus %>% 
  dplyr::select(ID, total_ct, accompanying_ct, following_wake_ct) %>% 
  dplyr::mutate(accompanying_ct = tidyr::replace_na(accompanying_ct, 0),
                following_wake_ct = tidyr::replace_na(following_wake_ct, 0))  %>%
  # Not necessarily an ID have only followers, so check it first
  dplyr::mutate(count_wo_followers = total_ct - (accompanying_ct + following_wake_ct)) %>%
  dplyr::filter(count_wo_followers == 0) %>%
  # Select IDs only with followers
  dplyr::select(ID)

# Exclude 'df_id_w_only_followers' from main dataset
df_EAus <- dplyr::anti_join(df_EAus, df_id_w_only_followers, by = "ID")

# Re-calculate 'total_ct' without followers again
df_EAus <- 
  df_EAus %>%
  dplyr::mutate(accompanying_ct = tidyr::replace_na(accompanying_ct, 0),
                following_wake_ct = tidyr::replace_na(following_wake_ct, 0))  %>%
  dplyr::mutate(total_ct = total_ct - (accompanying_ct + following_wake_ct))

## Keep only important columns
env_cols <- names(df_EAus[, c(42:54)])

df_EAus <- dplyr::select(df_EAus,
                         date, year, month, season, latitude, longitude, voyage,
                         order, family, genus, species, total_ct,
                         all_of(env_cols))

df_EAus$season <- factor(df_EAus$season, levels = c("summer", "autumn",
                                                    "winter", "spring"))

rm("df", "df_id_w_only_followers")

## *East Australia* raw data with env data & no followers -------------------- ##
readr::write_csv(df_EAus,
                 "./data_out/ASG_EAus_tidy-w-env.csv")

## Data - spatial objects ####

sf_EAus <- 
  df_EAus %>% dplyr::mutate(lon = longitude, lat = latitude) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview::mapview(sf_EAus)

## Load grids object
load("./data_out/grids_0.5-2.rda")

# # Select 'g1' (1 x 1 degree grid cells)
# g1 <- grids[["g1"]]

## Spatial join ####

grids_EAus <- lapply(grids, function(x) sf::st_join(sf_EAus, x))

## EDA - how many species by grid/season and grid size ####

df_grid_summary <- data.frame()

for (i in 1:length(grids_EAus)) {
  
  df <- 
    as.data.frame(grids_EAus[[i]]) %>%
    dplyr::group_by(IDgrid, season) %>%
    dplyr::summarise(n_birds = sum(total_ct),
                     n_spp = n_distinct(species)) %>%
    dplyr::mutate(grid = names(grids_EAus[i]))
  
  df_grid_summary <- rbind(df_grid_summary, df)
  
  rm("df", "i")
}

## Labels for plot
# how many different grids populated by season, by grid size
txt_hist <- 
  df_grid_summary %>% 
  dplyr::group_by(grid, season) %>% 
  dplyr::summarise(n_grids = paste0("(n = ", as.character(n_distinct(IDgrid)), ")")) %>%
  dplyr::ungroup(.) %>%
  dplyr::mutate(n_grids = paste(as.character(season), n_grids))

# how many different grids populated in total, by grid size
df_grid_summary %>% dplyr::group_by(grid) %>% dplyr::summarise(n_distinct(IDgrid))

gr_lab <- ggplot2::as_labeller(c(
  `g0.5`  = "g0.5 (n = 356)",
  `g0.75` = "g0.75 (n = 241)",
  `g1`    = "g1 (n = 172)",
  `g1.25` = "g1.25 (n = 142)",
  `g1.5`  = "g1.5 (n = 111)",
  `g2`    = "g2 (n = 78)"
))

plot_sp_richness_grids <-
  ggplot(df_grid_summary, aes(x = n_spp, fill = season)) +
  geom_histogram(binwidth = 1, alpha = 0.7) + 
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~ grid, labeller = gr_lab) +
  geom_text((txt_hist %>% dplyr::filter(season == "summer")), 
            mapping = aes(x = 22, y = 64, label = n_grids), size = 2) +  
  geom_text((txt_hist %>% dplyr::filter(season == "autumn")), 
            mapping = aes(x = 22, y = 58, label = n_grids), size = 2) +
  geom_text((txt_hist %>% dplyr::filter(season == "winter")), 
            mapping = aes(x = 22, y = 52, label = n_grids), size = 2) +  
  geom_text((txt_hist %>% dplyr::filter(season == "spring")), 
            mapping = aes(x = 22, y = 46, label = n_grids), size = 2) +
  ylab("Count") + xlab("Number of species per grid")+
  theme_bw() +
  theme(strip.text.x = element_text(size = 6),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.5, "cm"))

ggsave(plot_sp_richness_grids,
       filename = "./EDA/EDA_aggr_hist_0.5-2_all-seasons.png",
       height = 8, width = 12, units = "cm", dpi = 300)

rm("gr_lab", "txt_hist", "df_grid_summary", "plot_sp_richness_grids")

## Wrangling 'g1' for models ####

## Get aggregated data at 'g1' level
sf_EAus_g1 <- grids_EAus[["g1"]]

## Summarise mean and sd for all environmental variables, by season/grid
funs <- list(mean = ~ mean(.x, na.rm = TRUE),
             sd = ~ sd(.x, na.rm = TRUE))

env <-
  as.data.frame(sf::st_drop_geometry(sf_EAus_g1)) %>% 
  dplyr::group_by(IDgrid, season) %>%
  dplyr::summarise(across(all_of(env_cols), .fns = funs)) %>%
  dplyr::ungroup()

rm("funs")

## Get lon/lat for each grid centroid
latlon <- 
  grids[["g1"]] %>%
  sf::st_centroid() %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[, 1],
                lat = sf::st_coordinates(.)[, 2]) %>% 
  sf::st_drop_geometry()

## Summarise seabird data, by season/grid
seabirds <-
  as.data.frame(sf::st_drop_geometry(sf_EAus_g1)) %>% 
  dplyr::group_by(IDgrid, season, species) %>%
  dplyr::summarise(n = sum(total_ct)) %>%
  dplyr::ungroup()

##-----------------------------------------------------------------------------#
## Data for RCPs --------------------------------------------------------------#

## Pivot seabird species to columns
seabirds_rcp <- 
  seabirds %>%
  tidyr::pivot_wider(id_cols = c("IDgrid", "season"),
                     names_from = "species",
                     values_from = "n")

## Fill 'NA' with zeros
seabirds_rcp[is.na(seabirds_rcp)] <- 0

## Add grid centroid lon/lat
seabirds_rcp <- 
  dplyr::left_join(seabirds_rcp, latlon, by = c("IDgrid")) %>%
  dplyr::relocate(c(lon, lat), .after = season)

## Keep only "mean" cols from 'env' & join it with seabird data
names(env)

seabirds_rcp <- 
  dplyr::left_join(seabirds_rcp,
                   dplyr::select(.data = env, IDgrid, season, ends_with("_mean")),
                   by = c("IDgrid", "season"))

## Save it
readr::write_csv(seabirds_rcp, "./data_out/seabirds_rcp.csv")

## EDA - how environmental data varies between seasons ####

EDA_env <- 
  env[, 2:ncol(env)] %>%
  tidyr::pivot_longer(cols = !season, names_to = "vars", values_to = "value") %>%
  # filter cols that intrinsically varies a lot
  dplyr::filter(! vars == "bat_mean") %>% dplyr::filter(! vars == "bat_sd") %>%
  dplyr::filter(! vars == "dist_coast_mean") %>% dplyr::filter(! vars == "dist_coast_sd") %>%
  dplyr::filter(! vars == "mld_mean") %>% dplyr::filter(! vars == "mld_sd") %>%
  # quick boxplot
  ggplot() +
  geom_boxplot(aes(x = season, y = value, fill = season),
               lwd = 0.2, fatten = 0.7, outlier.size = 0.3) +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap("vars", scales = "free_y") +
  xlab("") + ylab("") + 
  theme_bw() +
  theme(strip.text.x = element_text(size = 6),
        axis.text = element_text(size = 6),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.key.size = unit(1, "cm"))

ggsave(EDA_env,
       filename = "./EDA/EDA_boxplot_env-by-seasons.png",
       height = 11, width = 16.5, units = "cm", dpi = 300)
