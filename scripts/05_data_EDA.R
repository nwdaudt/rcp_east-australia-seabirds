##
## ASG -- Region of Common Profile models
## 
## Nicholas W Daudt
## ****************************************************************************#

## This code makes some Exploratory Data Analysis (plots & maps)

## Libraries ####

library(plyr)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(RColorBrewer)
library(rnaturalearth)

## Data #### 

## Raw data - individual observations
data_raw <- read.csv("./data_out/ASG_EAus_tidy-w-env.csv")

data_raw$season <- factor(data_raw$season, levels = c("summer", "autumn",
                                                      "winter", "spring")) 
## Grids
load("./data_out/grids_0.5-2.rda")
g1 <- grids[["g1"]]

## Aggregated data
data_gridded <- read.csv("./data_out/seabirds_rcp.csv")
data_gridded <- head(data_gridded, -1)

spp_cols <- names(data_gridded[, 5:84])
env_cols <- names(data_gridded[, 85:ncol(data_gridded)])

## ------------------------------ EDA ------------------------------------- ####

## General summary #### 
length(unique(data_raw$voyage))   # > 15 voyages

min(data_raw$date); max(data_raw$date)
# > 2016-08-25; 2021-03-05

nrow(data_raw); plyr::count(data_raw$season)
# > 10,261 records, in total
# summer 3,076
# autumn 2,122
# winter 1,975
# spring 3,088

length(unique(data_raw$species))  # > 80 species
length(unique(data_raw$genus))    # > 35 genus
length(unique(data_raw$family))   # > 11 families
length(unique(data_raw$order))    # > 5 orders

sum(data_raw$total_ct)            # > In total, 142,646 individuals recorded

## Number of birds (total) by grid/season ----------------------------------####
df <- 
  data_gridded %>% 
  dplyr::mutate(sum_all = apply(data_gridded[spp_cols], 1, sum),
                spp_rich = apply(data_gridded[spp_cols], 1, function(x) sum(x >= 1)))

df$season <- factor(df$season, 
                    levels = c("summer", "autumn", "winter", "spring"))

quantile((df$sum_all))
# 0%     25%     50%     75%    100% 
# 1.0    10.0    45.5   216.0 14460.0

boxplot_sum.all_season <-
  ggplot(df, aes(x = season, y = sum_all)) +
  geom_boxplot(lwd = 0.3, outlier.size = 0.5) + 
  geom_hline(yintercept = 1000, colour = "red", linetype = "longdash") + 
  annotate("text", x = 4, y = 14000, label = "n = 1000", size = 3, colour = "red") +
  geom_hline(yintercept = 500, colour = "blue", linetype = "longdash") + 
  annotate("text", x = 4, y = 13000, label = "n = 500", size = 3, colour = "blue") +
  ylab("Total number of birds by grid") + xlab("") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 8),
        axis.text = element_text(size = 6))

ggsave(boxplot_sum.all_season, 
       filename = "./EDA/EDA_boxplot_total-count-seasons.png",
       height = 7, width = 7, units = "cm", dpi = 300)

## Number of birds by species/grid/season -----------------------####
df2 <- 
  df %>% 
  tidyr::pivot_longer(cols = all_of(spp_cols),
                      names_to = "spp",
                      values_to = "value")

boxplot_spp <-
  ggplot(df2, aes(x = spp, y = value)) +
  geom_boxplot(lwd = 0.25, outlier.size = 0.3) + 
  facet_wrap(~ season, ncol = 4, scales = "free_x") +
  coord_flip() + 
  geom_hline(yintercept = 1000, colour = "red", linetype = "longdash", size = 0.4) + 
  geom_hline(yintercept = 500, colour = "blue", linetype = "longdash", size = 0.4) + 
  ylab("Total number of birds by grid") + xlab("") +
  theme_bw() + 
  theme(axis.title = element_text(size = 8),
        axis.text.y = element_text(size = 4),
        axis.text.x = element_text(size = 6),
        strip.text = element_text(size = 8))

ggsave(boxplot_spp, 
       filename = "./EDA/EDA_boxplot_spp-seasons.png",
       height = 13, width = 20, units = "cm", dpi = 300)

## FO% (species-level) by grid/season --------------------------------------####
funs <- list(FO = ~ sum(.x >= 1)/n() *100,
             nOCC = ~ sum(.x >= 1))

df3 <- 
  df %>% 
  dplyr::group_by(season) %>%
  dplyr::summarise(across(all_of(spp_cols), .fns = funs)) %>%
  tidyr::pivot_longer(cols = !season, 
                      names_to = "spp",
                      values_to = "value") %>%
  # split name into variables
  tidyr::separate(spp, 
                  into = c("species", "vars"),
                  sep = "_")

plot_FO_nOCC <-
  ggplot(df3, aes(x = species, y = value, colour = vars, shape = vars)) + 
  geom_point(size = 1.2, alpha = 0.7) +
  scale_color_manual(values = c("#E69F00", "#56B4E9"))+
  facet_wrap(~season, ncol = 4, scales = "free_x") +
  coord_flip() + 
  geom_hline(yintercept = 10, colour = "#000000", linetype = "longdash", size = 0.4) + 
  # geom_hline(yintercept = 10, colour = "#56B4E9", linetype = "dashed") + 
  xlab("") + ylab("FO = frequency of occurrence (%)\n nOCC = number of occurrences") +
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 8),
        axis.text.y = element_text(size = 4),
        axis.text.x = element_text(size = 6),
        strip.text = element_text(size = 8))

ggsave(plot_FO_nOCC, 
       filename = "./EDA/EDA_spp-nOCC-FO-seasons.png",
       height = 13, width = 20, units = "cm", dpi = 300)

## String vector w/ spp names to remove from models & how many spp/season was modelled -----------####
## based on <6 occurrences

df_season_nOCC <-
  df3 %>% 
  dplyr::filter(vars == "nOCC")

remove_spp_vec <- list()

seasons_vec <- unique(df3$season)

for (season_vec in seasons_vec) {
  
  name <- paste0("spp_rm_", as.character(season_vec))
  
  vec <- 
    df_season_nOCC %>% 
    dplyr::filter(season == season_vec) %>%
    dplyr::filter(value < 6) %>%
    dplyr::pull(species)
  
  remove_spp_vec[[name]] <- vec
  
  rm("vec", "name", "season_vec")
}

## Number of species modelled by season **********

for (i in 1:length(remove_spp_vec)) {
  print(names(remove_spp_vec[i]))
  print(80 - (length(remove_spp_vec[[i]]))) # 80 = total no. of spp recorded
  rm("i")
}

# summer - 12 species
# autumn - 21
# winter - 20
# spring - 33

save("remove_spp_vec", file = "./data_out/rm_spp-season_vec.rda")

## Where the effort was -- individual records from raw data --------------------####

aus_sf <- rnaturalearth::ne_countries(country = "australia", returnclass = "sf")
world_sf <- ne_countries(returnclass = "sf")

# effort_raw_black <-
#   ggplot() +
#   geom_sf(data = aus_sf) +
#   # geom_sf(data = g1, aes(geometry = x), colour = "grey50", fill = NA) +
#   geom_point(data = data_raw,
#              mapping = aes(x = longitude, y = latitude),
#              size = 2, alpha = 0.6) + 
#   coord_sf(xlim = c(140, 165), ylim = c(-9, -48)) +
#   scale_x_continuous(breaks = c(145, 155, 165)) +
#   ylab("") + xlab("") +
#   theme_bw() + 
#   theme(axis.text = element_text(size = 6))

effort_raw_seasons <-
  ggplot() + 
  geom_sf(data = aus_sf) +
  geom_sf(data = g1, aes(geometry = x), colour = "grey50", fill = NA, size = 0.3) + 
  geom_point(data = data_raw[data_raw$longitude > 140, ], 
             mapping = aes(x = longitude, y = latitude, colour = season), 
             size = 0.8, alpha = 0.8) + 
  scale_color_brewer(palette = "Dark2", name = NULL) + 
  coord_sf(xlim = c(140, 165), ylim = c(-9, -48)) +
  scale_x_continuous(breaks = c(145, 155, 165)) +
  ylab("") + xlab("") +
  theme_bw() + 
  theme(legend.position = c(0.80, 0.18),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 6))

ggsave(effort_raw_seasons, 
       filename = "./EDA/EDA_map-effort-raw.png",
       height = 10, width = 8, units = "cm", dpi = 300)

# Save a map to scheme oceanography in Inkscape 
ggsave((ggplot() + 
          geom_sf(data = world_sf) +
          coord_sf(xlim = c(140, 177.2), ylim = c(-9, -48)) +
          scale_x_continuous(breaks = c(145, 155, 165, 175)) +
          theme_bw() + 
          theme(axis.text = element_text(size = 6)) + 
          ggspatial::annotation_scale(height = unit(0.15, "cm"),
                                      pad_x = unit(4.6, "cm"),
                                      pad_y = unit(7.7, "cm"),
                                      text_cex = 0.5) + 
          ggspatial::annotation_north_arrow(height = unit(0.55, "cm"),
                                            width = unit(0.4, "cm"),
                                            pad_x = unit(6.2, "cm"),
                                            pad_y = unit(0.4, "cm"),
                                            style = north_arrow_orienteering(text_size = 3))), 
       filename = "./EDA/EDA_map-for-oceanogr.svg", ## png
       height = 10, width = 8, units = "cm", dpi = 300)

## Where the effort was -- richness, by grid ------------------------------####

effort_richness <-
  ggplot(aus_sf) + 
  geom_sf() +
  geom_point(data = df, mapping = aes(x = lon, y = lat, colour = spp_rich), size = 1.5) + 
  scale_color_viridis_c(name = "# species") + 
  facet_wrap(~ season, ncol = 4) + 
  coord_sf(xlim = c(140, 161), ylim = c(-9, -48)) +
  scale_x_continuous(breaks = c(140, 150, 160)) +
  ylab("") + xlab("") +
  theme_bw() + 
  theme(legend.position = "right",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7.5),
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 6))

ggsave(effort_richness, 
       filename = "./EDA/EDA_map-effort-richness-seasons.png",
       height = 10, width = 20, units = "cm", dpi = 300)

## Where the effort was -- total numbers, by grid -------------------------####

effort_sum.all <-
  ggplot(aus_sf) + 
  geom_sf() +
  geom_point(data = df, mapping = aes(x = lon, y = lat, colour = sum_all), size = 1.5) + 
  scale_color_viridis_c(name = "# birds", option = "inferno") + 
  facet_wrap(~ season, ncol = 4) + 
  coord_sf(xlim = c(140, 161), ylim = c(-9, -48)) +
  scale_x_continuous(breaks = c(140, 150, 160)) +
  ylab("") + xlab("") +
  theme_bw() + 
  theme(legend.position = "right",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7.5),
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 6))

ggsave(effort_sum.all, 
       filename = "./EDA/EDA_map-effort-total-birds-recorded-seasons.png",
       height = 10, width = 20, units = "cm", dpi = 300)

## Supp Mat Fig: # richness & total # birds ####

SuppMat_Rich.Number <-
  effort_richness / effort_sum.all

ggsave(SuppMat_Rich.Number, 
       filename = "./EDA/EDA_map-effort-spp-richness-and-total-birds-seasons.png",
       height = 20, width = 20, units = "cm", dpi = 300)

## Clean environment -------------------------------------------------------####
rm(list = ls())

### Zeroes for each species/season? (probably don't need it)