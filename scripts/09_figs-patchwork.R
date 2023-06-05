##
## ASG -- Figures
## 
## Nicholas W Daudt
## ****************************************************************************#

## This code put together some Figs for the manuscript.

## Libraries ####

library(ggplot2)
library(patchwork)
library(magick)

## SVG

# https://cran.r-project.org/web/packages/grImport/vignettes/import.pdf
# grImport 

# https://github.com/m-jahn/fluctuator

## Figures #### 

## FIGURE 1 ------------------------------------------------------------------ #

# Based on:
# https://stackoverflow.com/questions/76079316/including-vector-graphics-file-svg-in-a-figure-generated-with-the-patchwork-pa

oceanogr <- magick::image_read_svg("./EDA/EDA_map-oceanogr1.svg")
gg_oceanogr <- magick::image_ggplot(oceanogr)

load("./EDA/EDA_map-effort-raw.rda")

fig1_study_area <-
  (gg_oceanogr + effort_raw_seasons) +
  patchwork::plot_layout(ncol = 2) + #, widths = c(2,1), heights = c(8,1)
  patchwork::plot_annotation(tag_levels = "a", tag_suffix = ")")

ggsave(fig1_study_area, 
       filename = "./results/Fig1_study-area-w-oceanogr.png",
       height = 10, width = 20, units = "cm", dpi = 300)

## FIGURE 2 ------------------------------------------------------------------ #

load("./results/Bernoulli/Bernoulli_07_RCP-point-predictions-ggplot.rda")
# As both files were originally names the same ('map_point_pred'), 
# create a new object for Bernoulli fig (_Ber)
map_point_pred_Ber <- map_point_pred

load("./results/NegBin/NegBin_07_RCP-point-predictions-ggplot.rda")

fig2_point_pred <-
  (map_point_pred_Ber + theme(panel.spacing.x = unit(0.8, "lines"))) / 
  (map_point_pred + theme(panel.spacing.x = unit(0.8, "lines"))) +
  patchwork::plot_layout(nrow = 2) +
  patchwork::plot_annotation(tag_levels = "a", tag_suffix = ")")

ggsave(fig2_point_pred, 
       filename = "./results/Fig2-point-predictions.png",
       height = 13, width = 15, units = "cm", dpi = 300)

## FIGURE 3 ------------------------------------------------------------------ #



