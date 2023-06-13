##
## ASG -- Figures & Tables
## 
## Nicholas W Daudt
## ****************************************************************************#

## This code put together some Figs & tables for the manuscript.

## Libraries ####

library(ggplot2)
library(patchwork)
library(magick)

## SVG

# https://cran.r-project.org/web/packages/grImport/vignettes/import.pdf
# grImport 

# https://github.com/m-jahn/fluctuator

## Figures main text #### 

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
  (map_point_pred_Ber + 
     theme(# panel.spacing.x = unit(0.8, "lines"),
           axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))) / 
  (map_point_pred + 
     theme(# panel.spacing.x = unit(0.8, "lines"),
           axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))) +
  patchwork::plot_layout(nrow = 2) +
  patchwork::plot_annotation(tag_levels = "a", tag_suffix = ")")

ggsave(fig2_point_pred, 
       filename = "./results/Fig2-point-predictions.png",
       height = 13, width = 15, units = "cm", dpi = 300)

rm("map_point_pred_Ber", "map_point_pred", "fig2_point_pred")

## FIGURE 3 ------------------------------------------------------------------ #

load("./results/Bernoulli/Bernoulli_spring_07_RCP-prob-predictions-ggplot.rda")
# As both files were originally names the same ('plotRCPs'), 
# create a new object for Bernoulli fig (_Ber)
plotRCPs_Ber <- plotRCPs

load("./results/NegBin/NegBin_spring_07_RCP-prob-predictions-ggplot.rda")

fig3_prob_maps_spring <- 
  (plotRCPs_Ber + theme(legend.position = "none")) / 
  plotRCPs +
  patchwork::plot_layout(ncol = 2) +
  patchwork::plot_annotation(tag_levels = "a", tag_suffix = ")")

ggsave(fig3_prob_maps_spring, 
       filename = "./results/Fig3-probability-maps-spring.png",
       height = 9, width = 16, units = "cm", dpi = 300)

rm("plotRCPs_Ber", "plotRCPs", "fig3_prob_maps_spring")

## FIGURE 4 ------------------------------------------------------------------ # Partial Plots -- COME BACK HERE!

# load("./results/Bernoulli/Bernoulli_spring_07_RCP-prob-predictions-ggplot.rda")
# # As both files were originally names the same ('plotRCPs'), 
# # create a new object for Bernoulli fig (_Ber)
# plotRCPs_Ber <- plotRCPs
# 
# load("./results/NegBin/NegBin_spring_07_RCP-prob-predictions-ggplot.rda")
# 
# fig4_partial_plots_spring <- 
#   (plotRCPs_Ber + theme(legend.position = "none")) / 
#   plotRCPs +
#   patchwork::plot_layout(ncol = 2) +
#   patchwork::plot_annotation(tag_levels = "a", tag_suffix = ")")
# 
# ggsave(fig4_partial_plots_spring, 
#        filename = "./results/Fig3-probability-maps-spring.png",
#        height = 9, width = 16, units = "cm", dpi = 300)

## FIGURE 5 ------------------------------------------------------------------ #

load("./results/Bernoulli/Bernoulli_spring_07_species-profiles-ggplot.rda")
# As both files were originally names the same ('plotRCPs'), 
# create a new object for Bernoulli fig (_Ber)
plot_spp_profiles_CI_Ber <- plot_spp_profiles_CI

load("./results/NegBin/NegBin_spring_07_species-profiles-ggplot.rda")

fig5_spp_profiles <- 
  (plot_spp_profiles_CI_Ber + xlab("Probability of Occurrence\n + Confidence Interval") + theme(axis.text.y = element_text(size = 5))) / 
  (plot_spp_profiles_CI + xlab("log(Predicted mean abundance)\n + Confidence Interval") + theme(axis.text.y = element_blank())) +
  patchwork::plot_layout(ncol = 2) +
  patchwork::plot_annotation(tag_levels = "a", tag_suffix = ")")

ggsave(fig5_spp_profiles, 
       filename = "./results/Fig5-spp-profiles-spring.png",
       height = 9, width = 16, units = "cm", dpi = 300)

rm("plot_spp_profiles_CI_Ber", "plot_spp_profiles_CI", "fig5_spp_profiles")

## Figures supplementary material ####

## FIGURE S1 & FIGURE S2 ----------------------------------------------------- #

# --- these Figs were done under script '05_data_EDA.R' ---

## FIGURE S3 ----------------------------------------------------------------- #

## -------------------- S3.1 Bernoulli -------------------------------------- ##

load("./results/Bernoulli/Bernoulli_summer_01_multifit-plot-ggplot.rda")
gg_multifitBIC_sum <- gg_multifitBIC

load("./results/Bernoulli/Bernoulli_autumn_01_multifit-plot-ggplot.rda")
gg_multifitBIC_aut <- gg_multifitBIC

load("./results/Bernoulli/Bernoulli_winter_01_multifit-plot-ggplot.rda")
gg_multifitBIC_win <- gg_multifitBIC

load("./results/Bernoulli/Bernoulli_spring_01_multifit-plot-ggplot.rda")

figS3.1_multifit <-
  (gg_multifitBIC_sum + ggtitle("Summer") + 
     theme(plot.title = element_text(size = 7, face = "italic"))) +
  (gg_multifitBIC_aut + ggtitle("Autumn") + 
     theme(plot.title = element_text(size = 7, face = "italic"),
           legend.position = "none")) + 
  (gg_multifitBIC_win + ggtitle("Winter") + 
     theme(plot.title = element_text(size = 7, face = "italic"),
           legend.position = "none")) + 
  (gg_multifitBIC + ggtitle("Spring") + 
     theme(plot.title = element_text(size = 7, face = "italic"),
           legend.position = "none")) + 
  patchwork::plot_layout(nrow = 4)

ggsave(figS3.1_multifit, 
       filename = "./results/FigS3_1_multifit-Bernoulli.png",
       height = 18, width = 13, units = "cm", dpi = 300)

## ---------------------- S3.2 NegBin --------------------------------------- ##
##      Note: I'm overwriting previous objects by using the same names

load("./results/NegBin/NegBin_summer_01_multifit-plot-ggplot.rda")
gg_multifitBIC_sum <- gg_multifitBIC

load("./results/NegBin/NegBin_autumn_01_multifit-plot-ggplot.rda")
gg_multifitBIC_aut <- gg_multifitBIC

load("./results/NegBin/NegBin_winter_01_multifit-plot-ggplot.rda")
gg_multifitBIC_win <- gg_multifitBIC

load("./results/NegBin/NegBin_spring_01_multifit-plot-ggplot.rda")

figS3.2_multifit <-
  (gg_multifitBIC_sum + ggtitle("Summer") + 
     theme(plot.title = element_text(size = 7, face = "italic"))) +
  (gg_multifitBIC_aut + ggtitle("Autumn") + 
     theme(plot.title = element_text(size = 7, face = "italic"),
           legend.position = "none")) + 
  (gg_multifitBIC_win + ggtitle("Winter") + 
     theme(plot.title = element_text(size = 7, face = "italic"),
           legend.position = "none")) + 
  (gg_multifitBIC + ggtitle("Spring") + 
     theme(plot.title = element_text(size = 7, face = "italic"),
           legend.position = "none")) + 
  patchwork::plot_layout(nrow = 4)

ggsave(figS3.2_multifit, 
       filename = "./results/FigS3_2_multifit-NegBin.png",
       height = 18, width = 13, units = "cm", dpi = 300)

rm("gg_multifitBIC_sum", "gg_multifitBIC_aut", "gg_multifitBIC_win", "gg_multifitBIC",
   "figS3.1_multifit", "figS3.2_multifit")

## FIGURE S4 ----------------------------------------------------------------- #

## --------------------- S4.1 Bernoulli ------------------------------------- ##

sum <- 
  magick::image_read("./results/Bernoulli/Bernoulli_summer_05_best-model-residuals.png") %>% 
  magick::image_ggplot()
aut <- 
  magick::image_read("./results/Bernoulli/Bernoulli_autumn_05_best-model-residuals.png") %>% 
  magick::image_ggplot()
win <- 
  magick::image_read("./results/Bernoulli/Bernoulli_winter_05_best-model-residuals.png") %>% 
  magick::image_ggplot()
spr <- 
  magick::image_read("./results/Bernoulli/Bernoulli_spring_05_best-model-residuals.png") %>% 
  magick::image_ggplot()

ggsave(
  gridExtra::grid.arrange(
    (sum + ggtitle("Summer") + theme(plot.title = element_text(size = 7, face = "italic"))), 
    (aut + ggtitle("Autumn") + theme(plot.title = element_text(size = 7, face = "italic"))), 
    (win + ggtitle("Winter") + theme(plot.title = element_text(size = 7, face = "italic"))), 
    (spr + ggtitle("Spring") + theme(plot.title = element_text(size = 7, face = "italic"))),
    nrow = 4),
  filename = "./results/FigS4_1_best-model-residuals-Bernoulli.png",
  height = 16, width = 10, units = "cm", dpi = 300
)

## ---------------------- S3.2 NegBin --------------------------------------- ##
##      Note: I'm overwriting previous objects by using the same names

sum <- 
  magick::image_read("./results/NegBin/NegBin_summer_05_best-model-residuals.png") %>% 
  magick::image_ggplot()
aut <- 
  magick::image_read("./results/NegBin/NegBin_autumn_05_best-model-residuals.png") %>% 
  magick::image_ggplot()
win <- 
  magick::image_read("./results/NegBin/NegBin_winter_05_best-model-residuals.png") %>% 
  magick::image_ggplot()
spr <- 
  magick::image_read("./results/NegBin/NegBin_spring_05_best-model-residuals.png") %>% 
  magick::image_ggplot()

ggsave(
  gridExtra::grid.arrange(
    (sum + ggtitle("Summer") + theme(plot.title = element_text(size = 7, face = "italic"))), 
    (aut + ggtitle("Autumn") + theme(plot.title = element_text(size = 7, face = "italic"))), 
    (win + ggtitle("Winter") + theme(plot.title = element_text(size = 7, face = "italic"))), 
    (spr + ggtitle("Spring") + theme(plot.title = element_text(size = 7, face = "italic"))),
    nrow = 4),
  filename = "./results/FigS4_2_best-model-residuals-NegBin.png",
  height = 16, width = 10, units = "cm", dpi = 300
)

rm("sum", "aut", "win", "spr")

