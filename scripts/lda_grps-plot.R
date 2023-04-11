##
## ASG -- LDA attempt, mapping groups
## 
## Nicholas W Daudt
## ****************************************************************************#

## @ LDA map

## Libraries ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(tmap)
library(sf)
library(rnaturalearth)

## Data ####

# df <- read.csv("./data_out/inputs/seabirds_rcp.csv")
# df <- dplyr::filter(df, season == "spring")

## LDA probs
df_probs <- read.csv("./data_out/lda_group_prob.csv")

## SPP probs
df_probs_spp <- read.csv("./data_out/lda_group_comp.csv")

## Load grids object
load("./data_out/inputs/grids_0.5-2.rda")

## Get lon/lat for each grid centroid
latlon <- 
  grids[["g1"]] #%>%
  # sf::st_centroid() %>%
  # dplyr::mutate(lon = sf::st_coordinates(.)[, 1],
  #               lat = sf::st_coordinates(.)[, 2]) %>% 
  # sf::st_drop_geometry()

## Merge probs and grid polygons
sf_LDA_probs <- merge(latlon, df_probs, by = "IDgrid")
# mapview::mapview(sf_LDA_probs) # worked

## Tranform it for long-format
sf_LDA_probs <-
  sf_LDA_probs %>%
  tidyr::pivot_longer(cols = c("gprob.1", "gprob.2", "gprob.3"),
                      names_to = "grps",
                      values_to = "values")

## Map #### 
aus_sf <- rnaturalearth::ne_countries(country = "australia", returnclass = "sf")

map <-
  # Base map
  tm_shape(aus_sf) + tm_polygons() +
  # LDA probs
  tm_shape(sf_LDA_probs) + 
  tm_polygons("values", palette = viridis::viridis(5)) +
  tm_facets("grps", ncol = 3) +
  tm_layout(panel.label.size = 2.5,
            legend.title.size = 1,
            legend.text.size = 1,
            outer.margins = c(0.05,0.05,0.05,0),
            asp = 0)

tmap_save(tm = map,
          filename = "./figs/LDa_trial-spp.pdf",
          width = 20, height = 15, units = "cm",
          dpi = 250)

## Spp profiles ####

df_probs_spp <- 
  df_probs_spp %>%
  dplyr::mutate(grps = c("grp.1", "grp.2", "grp.3"), .before = everything()) %>%
  tidyr::pivot_longer(cols = !grps,
                      names_to = "spp",
                      values_to = "value")

grps_comp <-
  ggplot(df_probs_spp, aes(x = spp, y = value, fill = grps)) +
  geom_col() + 
  scale_fill_manual(values = c("#000000", "#E69F00", "#56B4E9")) + ##999999
  facet_wrap(~ grps, ncol = 3) +
  xlab("") + ylab("") + 
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 8))

ggsave(grps_comp,
       filename = "./figs/LDA_trial-spp-comp-spring.pdf",
       width = 30, height = 20, units = "cm", dpi = 300)


