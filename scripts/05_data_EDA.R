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

## Effort summary, by voyage (Table S1) ####

tableS1 <-
  data_raw %>% 
  dplyr::group_by(voyage) %>% 
  dplyr::summarise(date_start = min(date),
                   date_end = max(date),
                   lat_range = paste(round(min(latitude)), "–", round(max(latitude))),
                   lon_range = paste(round(min(longitude)), "–", round(max(longitude))),
                   no_records = n(),
                   no_birds = sum(total_ct),
                   no_spp = n_distinct(species)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(date_start)

write.csv(tableS1, "./results/tableS1-effort-summary.csv")

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

# ggsave(boxplot_sum.all_season, 
#        filename = "./EDA/EDA_boxplot_total-count-seasons.png",
#        height = 7, width = 7, units = "cm", dpi = 300)

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

# ggsave(boxplot_spp, 
#        filename = "./EDA/EDA_boxplot_spp-seasons.png",
#        height = 13, width = 20, units = "cm", dpi = 300)

## Number of occurrences, Frequency of Occurrence, Numeric Frequency by grid/season (Figure S1) ####

funs <- list(
  # Frequency of Occurrence
  FO = ~ round(sum(.x >= 1) / n() *100, digits = 1),
  # Number of Occurrences
  nOCC = ~ sum(.x >= 1),
  # Numeric Frequency
  NF = ~ round((sum(.x) / sum(dplyr::pick(sum_all))) *100, digits = 2)
)

df3 <- 
  df %>% 
  # Create a 'total' in season for running the descriptive stats from 'funs'
  rbind((df %>% dplyr::mutate(season = as.factor("total")))) %>%
  # Then, run the descriptive stats
  dplyr::group_by(season) %>%
  dplyr::summarise(across(all_of(spp_cols), .fns = funs)) %>%
  # Longer format
  tidyr::pivot_longer(cols = !season, 
                      names_to = "spp",
                      values_to = "value") %>%
  # Split name into variables
  tidyr::separate(spp, 
                  into = c("species", "vars"),
                  sep = "_")

# Factor cols
df3$vars <- factor(df3$vars, levels = c("nOCC", "FO", "NF"))
df3$season <- factor(df3$season, levels = c("total", "summer", "autumn", "winter", "spring"))

## Get the order of the most frequent taxa to re-order below

## FO = frequency of occurrence
spp_FO_order <-
  df3 %>%
  dplyr::filter(vars == "FO") %>%
  dplyr::filter(season == "total") %>%
  dplyr::arrange(desc(value)) %>%
  dplyr::pull(species)

## nOCC = number of occurrences
# spp_nOCC_order <-
#   df3 %>%
#   dplyr::filter(vars == "nOCC") %>%
#   dplyr::filter(season == "total") %>%
#   dplyr::arrange(desc(value))

## NF = numeric frequency
# spp_NF_order <-
#   df3 %>%
#   dplyr::filter(vars == "NF") %>%
#   dplyr::filter(season == "total") %>%
#   dplyr::arrange(desc(value))

df3$species <- factor(df3$species, levels = spp_FO_order)
# levels(df3$species)

plot_nOCC_FO_NF <-
  ggplot(df3, aes(x = species, y = value, colour = vars, shape = vars)) + 
  geom_point(size = 1.2, alpha = 0.7) +
  scale_color_manual(values = c("#000000", "#E69F00", "#56B4E9")) +
  scale_x_discrete(limits = rev) + 
  facet_wrap(~ season, ncol = 5, scales = "free_x") +
  coord_flip() + 
  geom_hline(yintercept = 6, colour = "grey15", linetype = "longdash", linewidth = 0.4) + 
  xlab("") + 
  ylab("nOCC = number of occurrences\n FO = frequency of occurrence (%)\n NF = numeric frequency (%)") +
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 8),
        axis.text.y = element_text(size = 4),
        axis.text.x = element_text(size = 6),
        strip.text = element_text(size = 8))

ggsave(plot_nOCC_FO_NF, 
       filename = "./results/FigS1_spp-nOCC-FO-NF-seasons.png",
       height = 14, width = 20, units = "cm", dpi = 300)

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

save("effort_raw_seasons", file = "./EDA/EDA_map-effort-raw.rda")

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
       filename = "./EDA/EDA_map-oceanogr.svg", ## png
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

## patchwork #richness & #birds (Figure S2) ####

SuppMat_Rich.Number <-
  effort_richness / effort_sum.all +
  patchwork::plot_annotation(tag_levels = "a", tag_suffix = ")")

ggsave(SuppMat_Rich.Number, 
       filename = "./results/FigS2_map-effort-spp-richness-and-total-birds-seasons.png",
       height = 20, width = 20, units = "cm", dpi = 300)

## Clean environment -------------------------------------------------------####
rm(list = ls())

