##
## ASG -- {patchwork} figures for final MS & Supp Mat
## 
## *************************************************************************** #

## This code put together the Figs for the manuscript.

## Libraries ####

library(ggplot2)
library(patchwork)
library(magick)

## Figures main text #### 

### FIGURE 1 -------------------------------------------------------------- ####

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

## The quality of this plot was not great. So I ended up using a .svg version
## that I put both maps together by hand in Inkscape software

### FIGURE 2 -------------------------------------------------------------- ####

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
       filename = "./results/Fig2_point-predictions.png",
       height = 15, width = 11.5, units = "cm", dpi = 300)

rm("map_point_pred_Ber", "map_point_pred", "fig2_point_pred")

### FIGURE 3 --------------------------------------------------------------- ####

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
       filename = "./results/Fig3_probability-maps-spring.png",
       height = 9, width = 16, units = "cm", dpi = 300)

rm("plotRCPs_Ber", "plotRCPs", "fig3_prob_maps_spring")

### FIGURE 4 -------------------------------------------------------------- ####

B_spr_bat <- 
  magick::image_read("./results/Bernoulli/Bernoulli_spring_07_partial-plot-bat.png") %>% 
  magick::image_ggplot()
B_spr_sst <- 
  magick::image_read("./results/Bernoulli/Bernoulli_spring_07_partial-plot-sst.png") %>% 
  magick::image_ggplot()

B_spr <- B_spr_bat | B_spr_sst

NB_spr_sst <- 
  magick::image_read("./results/NegBin/NegBin_spring_07_partial-plot-sst.png") %>% 
  magick::image_ggplot()
NB_spr_clim_eke_mean <- 
  magick::image_read("./results/NegBin/NegBin_spring_07_partial-plot-clim-eke-mean.png") %>% 
  magick::image_ggplot()

NB_spr <- NB_spr_sst | NB_spr_clim_eke_mean

fig4_partial_plots_spring <-
  B_spr / NB_spr +
  patchwork::plot_annotation(tag_levels = list(c("a)", "", "b)", "")))

ggsave(fig4_partial_plots_spring,
       filename = "./results/Fig4_partial-plots-spring.png",
       height = 11, width = 11, units = "cm", dpi = 300)

rm("B_spr_bat", "B_spr_sst", "NB_spr_sst", "NB_spr_clim_eke_mean",
   "fig4_partial_plots_spring")

### FIGURE 5 -------------------------------------------------------------- ####

load("./results/Bernoulli/Bernoulli_spring_07_species-profiles-ggplot.rda")
# As both files were originally names the same ('plotRCPs'), 
# create a new object for Bernoulli fig (_Ber)
plot_spp_profiles_CI_Ber <- plot_spp_profiles_CI

load("./results/NegBin/NegBin_spring_07_species-profiles-ggplot.rda")

fig5_spp_profiles <- 
  (plot_spp_profiles_CI_Ber + xlab("Probability of occurrence\n + Confidence interval") + theme(axis.text.y = element_text(size = 8),
                                                                                                axis.title.x = element_text(size = 7.5))) / 
  (plot_spp_profiles_CI + xlab("log(Predicted mean abundance)\n + Confidence interval") + theme(axis.text.y = element_blank(),
                                                                                                axis.title.x = element_text(size = 7.5))) +
  patchwork::plot_layout(ncol = 2) +
  patchwork::plot_annotation(tag_levels = "a", tag_suffix = ")")

ggsave(fig5_spp_profiles, 
       filename = "./results/Fig5_spp-profiles-spring.png",
       height = 12, width = 16, units = "cm", dpi = 300)

rm("plot_spp_profiles_CI_Ber", "plot_spp_profiles_CI", "fig5_spp_profiles")

## Figures supplementary material ####

### FIGURE S1 & FIGURE S2 ------------------------------------------------- ####

# --- these Figs were done under script '05_data_EDA.R' ---

### FIGURE S3 ------------------------------------------------------------- ####

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

### FIGURE S4 ------------------------------------------------------------- ####

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

### FIGURE S5 ------------------------------------------------------------- ####

## --------------------- S5.1 Bernoulli ------------------------------------- ##

load("./results/Bernoulli/Bernoulli_summer_07_RCP-prob-predictions-ggplot.rda")
plotRCPs_sum <- plotRCPs

load("./results/Bernoulli/Bernoulli_autumn_07_RCP-prob-predictions-ggplot.rda")
plotRCPs_aut <- plotRCPs

load("./results/Bernoulli/Bernoulli_winter_07_RCP-prob-predictions-ggplot.rda")
plotRCPs_win <- plotRCPs

load("./results/Bernoulli/Bernoulli_spring_07_RCP-prob-predictions-ggplot.rda")

figS5.1_prob_pred <-
  (plotRCPs_sum + ggtitle("Summer") + 
     theme(plot.title = element_text(size = 8, face = "italic"),
           legend.position = "none",
           axis.text = element_text(size = 3.5),
           strip.text = element_text(size = 4))) +
  (plotRCPs_aut + ggtitle("Autumn") + 
     theme(plot.title = element_text(size = 8, face = "italic"),
           legend.position = "none",
           axis.text = element_text(size = 3.5),
           strip.text = element_text(size = 4))) + 
  (plotRCPs_win + ggtitle("Winter") + 
     theme(plot.title = element_text(size = 8, face = "italic"),
           legend.position = "none",
           axis.text = element_text(size = 3.5),
           strip.text = element_text(size = 4))) + 
  (plotRCPs + ggtitle("Spring") + 
     theme(plot.title = element_text(size = 8, face = "italic"),
           axis.text = element_text(size = 3.5),
           strip.text = element_text(size = 4),
           legend.key.size = unit(0.3, 'cm'),
           legend.text = element_text(size = 4),
           legend.title = element_text(size = 5))) + 
  patchwork::plot_layout(ncol = 4)

ggsave(figS5.1_prob_pred, 
       filename = "./results/FigS5_1_prob-pred-Bernoulli.png",
       height = 9, width = 20, units = "cm", dpi = 300)

## ---------------------- S5.2 NegBin --------------------------------------- ##
##      Note: I'm overwriting previous objects by using the same names

load("./results/NegBin/NegBin_summer_07_RCP-prob-predictions-ggplot.rda")
plotRCPs_sum <- plotRCPs

load("./results/NegBin/NegBin_autumn_07_RCP-prob-predictions-ggplot.rda")
plotRCPs_aut <- plotRCPs

load("./results/NegBin/NegBin_winter_07_RCP-prob-predictions-ggplot.rda")
plotRCPs_win <- plotRCPs

load("./results/NegBin/NegBin_spring_07_RCP-prob-predictions-ggplot.rda")

figS5.2_prob_pred <-
  (plotRCPs_sum + ggtitle("Summer") + 
     theme(plot.title = element_text(size = 8, face = "italic"),
           legend.position = "none",
           axis.text = element_text(size = 3.5),
           strip.text = element_text(size = 4))) +
  (plotRCPs_aut + ggtitle("Autumn") + 
     theme(plot.title = element_text(size = 8, face = "italic"),
           legend.position = "none",
           axis.text = element_text(size = 3.5),
           strip.text = element_text(size = 4))) + 
  (plotRCPs_win + ggtitle("Winter") + 
     theme(plot.title = element_text(size = 8, face = "italic"),
           legend.position = "none",
           axis.text = element_text(size = 3.5),
           strip.text = element_text(size = 4))) + 
  (plotRCPs + ggtitle("Spring") + 
     theme(plot.title = element_text(size = 8, face = "italic"),
           axis.text = element_text(size = 3.5),
           strip.text = element_text(size = 4),
           legend.key.size = unit(0.3, 'cm'),
           legend.text = element_text(size = 4),
           legend.title = element_text(size = 5))) + 
  patchwork::plot_layout(ncol = 4)

ggsave(figS5.2_prob_pred, 
       filename = "./results/FigS5_2_prob-pred-NegBin.png",
       height = 8, width = 20, units = "cm", dpi = 300)

rm("plotRCPs_sum", "plotRCPs_aut", "plotRCPs_win", "plotRCPs",
   "figS5.1_prob_pred", "figS5.2_prob_pred")

### FIGURE S6 ------------------------------------------------------------- ####

## --------------------- S6.1 Bernoulli ------------------------------------- ##

B_sum_1 <- 
  magick::image_read("./results/Bernoulli/Bernoulli_summer_07_partial-plot-bat.png") %>% 
  magick::image_ggplot() + 
  ggtitle("Summer") + theme(plot.title = element_text(size = 4.5, face = "italic"))
B_sum_2 <- 
  magick::image_read("./results/Bernoulli/Bernoulli_summer_07_partial-plot-sss.png") %>% 
  magick::image_ggplot()

B_aut_1 <- 
  magick::image_read("./results/Bernoulli/Bernoulli_autumn_07_partial-plot-bat.png") %>% 
  magick::image_ggplot() + 
  ggtitle("Autumn") + theme(plot.title = element_text(size = 4.5, face = "italic"))
B_aut_2 <- 
  magick::image_read("./results/Bernoulli/Bernoulli_autumn_07_partial-plot-sst.png") %>% 
  magick::image_ggplot()

B_win_1 <- 
  magick::image_read("./results/Bernoulli/Bernoulli_winter_07_partial-plot-sst.png") %>% 
  magick::image_ggplot() + 
  ggtitle("Winter") + theme(plot.title = element_text(size = 4.5, face = "italic"))

B_spr_1 <- 
  magick::image_read("./results/Bernoulli/Bernoulli_spring_07_partial-plot-bat.png") %>% 
  magick::image_ggplot() + 
  ggtitle("Spring") + theme(plot.title = element_text(size = 4.5, face = "italic"))
B_spr_2 <- 
  magick::image_read("./results/Bernoulli/Bernoulli_spring_07_partial-plot-sst.png") %>% 
  magick::image_ggplot()

B_sum <- B_sum_1 + B_sum_2
B_aut <- B_aut_1 + B_aut_2
B_win <- B_win_1 #+ patchwork::plot_spacer()
B_spr <- B_spr_1 + B_spr_2

layout = "
          AA
          BB
          CC
          DD
         "

figS6.1_partial_plots_Bernoulli <-
  B_sum / 
  B_aut / 
  B_win /
  B_spr +
  patchwork::plot_layout(design = layout)

ggsave(figS6.1_partial_plots_Bernoulli,
       filename = "./results/FigS6_1_partial-plots-Bernoulli.png",
       height = 16, width = 10, units = "cm", dpi = 300)

rm("B_sum_1", "B_sum_2", 
   "B_aut_1", "B_aut_2", 
   "B_win_1", 
   "B_spr_1", "B_spr_2", 
   "B_sum", "B_aut", "B_win", "B_spr",
   "figS6.1_partial_plots_Bernoulli",
   "layout")

## ---------------------- S6.2 NegBin --------------------------------------- ##
##      Note: I'm overwriting previous objects by using the same names

NB_sum_1 <- 
  magick::image_read("./results/NegBin/NegBin_summer_07_partial-plot-bat.png") %>% 
  magick::image_ggplot() + 
  ggtitle("Summer") + theme(plot.title = element_text(size = 4.5, face = "italic"))
NB_sum_2 <- 
  magick::image_read("./results/NegBin/NegBin_summer_07_partial-plot-sss.png") %>% 
  magick::image_ggplot()

NB_aut_1 <- 
  magick::image_read("./results/NegBin/NegBin_autumn_07_partial-plot-bat.png") %>% 
  magick::image_ggplot() + 
  ggtitle("Autumn") + theme(plot.title = element_text(size = 4.5, face = "italic"))
NB_aut_2 <- 
  magick::image_read("./results/NegBin/NegBin_autumn_07_partial-plot-chl.png") %>% 
  magick::image_ggplot()
NB_aut_3 <- 
  magick::image_read("./results/NegBin/NegBin_autumn_07_partial-plot-sst.png") %>% 
  magick::image_ggplot()
NB_aut_4 <- 
  magick::image_read("./results/NegBin/NegBin_autumn_07_partial-plot-mld.png") %>% 
  magick::image_ggplot()
NB_aut_5 <- 
  magick::image_read("./results/NegBin/NegBin_autumn_07_partial-plot-clim-sst-grad.png") %>% 
  magick::image_ggplot()

NB_win_1 <- 
  magick::image_read("./results/NegBin/NegBin_winter_07_partial-plot-sst.png") %>% 
  magick::image_ggplot() + 
  ggtitle("Winter") + theme(plot.title = element_text(size = 4.5, face = "italic"))

NB_spr_1 <- 
  magick::image_read("./results/NegBin/NegBin_spring_07_partial-plot-sst.png") %>% 
  magick::image_ggplot() + 
  ggtitle("Spring") + theme(plot.title = element_text(size = 4.5, face = "italic"))
NB_spr_2 <- 
  magick::image_read("./results/NegBin/NegBin_spring_07_partial-plot-clim-eke-mean.png") %>% 
  magick::image_ggplot()

NB_sum <- NB_sum_1 + NB_sum_2
NB_aut <- (NB_aut_1 + NB_aut_2 + NB_aut_3 + NB_aut_4 + NB_aut_5) + patchwork::plot_layout(ncol = 5)
NB_win <- NB_win_1 #+ patchwork::plot_spacer()
NB_spr <- NB_spr_1 + NB_spr_2

layout = "
          AA###
          BBBBB
          CC#DD
         "

figS6.2_partial_plots_NegBin <-
  NB_sum / 
  NB_aut / 
  NB_win /
  NB_spr +
  patchwork::plot_layout(design = layout)

ggsave(figS6.2_partial_plots_NegBin,
       filename = "./results/FigS6_2_partial-plots-NegBin.png",
       height = 11, width = 16, units = "cm", dpi = 300)

rm("NB_sum", "NB_sum_1", "NB_sum_2",
   "NB_aut", "NB_aut_1", "NB_aut_2", "NB_aut_3", "NB_aut_4", "NB_aut_5",
   "NB_win", "NB_win_1",
   "NB_spr", "NB_spr_1", "NB_spr_2",
   "figS6.2_partial_plots_NegBin",
   "layout")

### FIGURE S7 ------------------------------------------------------------- ####

## --------------------- S7.1 Bernoulli ------------------------------------- ##

load("./results/Bernoulli/Bernoulli_summer_07_species-profiles-ggplot.rda")
plot_spp_profiles_CI_sum <- plot_spp_profiles_CI

load("./results/Bernoulli/Bernoulli_autumn_07_species-profiles-ggplot.rda")
plot_spp_profiles_CI_aut <- plot_spp_profiles_CI

load("./results/Bernoulli/Bernoulli_winter_07_species-profiles-ggplot.rda")
plot_spp_profiles_CI_win <- plot_spp_profiles_CI

load("./results/Bernoulli/Bernoulli_spring_07_species-profiles-ggplot.rda")

figS7.1_spp_profiles <-
  ((plot_spp_profiles_CI_sum + 
      ggtitle("Summer") + theme(plot.title = element_text(size = 8, face = "italic"),
                                strip.text = element_text(size = 6),
                                axis.text.x = element_text(size = 5),
                                axis.text.y = element_text(size = 6),
                                axis.title.x = element_text(size = 6))) + 
     (plot_spp_profiles_CI_aut + 
        ggtitle("Autumn") + theme(plot.title = element_text(size = 8, face = "italic"),
                                  strip.text = element_text(size = 6),
                                  axis.text.x = element_text(size = 5),
                                  axis.text.y = element_text(size = 6),
                                  axis.title.x = element_text(size = 6)))) /
  ((plot_spp_profiles_CI_win + 
      ggtitle("Winter") + theme(plot.title = element_text(size = 8, face = "italic"),
                                strip.text = element_text(size = 6),
                                axis.text.x = element_text(size = 5),
                                axis.text.y = element_text(size = 6),
                                axis.title.x = element_text(size = 6))) +
     (plot_spp_profiles_CI + 
        ggtitle("Spring") + theme(plot.title = element_text(size = 8, face = "italic"),
                                  strip.text = element_text(size = 6),
                                  axis.text.x = element_text(size = 5),
                                  axis.text.y = element_text(size = 6),
                                  axis.title.x = element_text(size = 6))))

ggsave(figS7.1_spp_profiles, 
       filename = "./results/FigS7_1_spp-profiles-Bernoulli.png",
       height = 15, width = 20, units = "cm", dpi = 300)

## ---------------------- S7.2 NegBin --------------------------------------- ##
##      Note: I'm overwriting previous objects by using the same names

load("./results/NegBin/NegBin_summer_07_species-profiles-ggplot.rda")
plot_spp_profiles_CI_sum <- plot_spp_profiles_CI

load("./results/NegBin/NegBin_autumn_07_species-profiles-ggplot.rda")
plot_spp_profiles_CI_aut <- plot_spp_profiles_CI

load("./results/NegBin/NegBin_winter_07_species-profiles-ggplot.rda")
plot_spp_profiles_CI_win <- plot_spp_profiles_CI

load("./results/NegBin/NegBin_spring_07_species-profiles-ggplot.rda")

figS7.2_spp_profiles <-
  ((plot_spp_profiles_CI_sum + 
      ggtitle("Summer") + xlab("log(Predicted mean abundance) + Confidence interval") + 
      theme(plot.title = element_text(size = 8, face = "italic"),
            strip.text = element_text(size = 6),
            axis.text.x = element_text(size = 5),
            axis.text.y = element_text(size = 6),
            axis.title.x = element_text(size = 6))) + 
     (plot_spp_profiles_CI_aut + 
        ggtitle("Autumn") + xlab("log(Predicted mean abundance) + Confidence interval") + 
        theme(plot.title = element_text(size = 8, face = "italic"),
              strip.text = element_text(size = 6),
              axis.text.x = element_text(size = 5),
              axis.text.y = element_text(size = 6),
              axis.title.x = element_text(size = 6)))) /
  ((plot_spp_profiles_CI_win + 
      ggtitle("Winter") + xlab("log(Predicted mean abundance) + Confidence interval") + 
      theme(plot.title = element_text(size = 8, face = "italic"),
            strip.text = element_text(size = 6),
            axis.text.x = element_text(size = 5),
            axis.text.y = element_text(size = 6),
            axis.title.x = element_text(size = 6))) +
     (plot_spp_profiles_CI + 
        ggtitle("Spring") + xlab("log(Predicted mean abundance) + Confidence interval") + 
        theme(plot.title = element_text(size = 8, face = "italic"),
              strip.text = element_text(size = 6),
              axis.text.x = element_text(size = 5),
              axis.text.y = element_text(size = 6),
              axis.title.x = element_text(size = 6))))

ggsave(figS7.2_spp_profiles, 
       filename = "./results/FigS7_2_spp-profiles-NegBin.png",
       height = 15, width = 20, units = "cm", dpi = 300)

rm("plot_spp_profiles_CI_sum", "plot_spp_profiles_CI_aut", 
   "plot_spp_profiles_CI_win", "plot_spp_profiles_CI",
   "figS7.1_spp_profiles", "figS7.2_spp_profiles")

### FIGURE S8 ------------------------------------------------------------- ####

sum <- 
  magick::image_read("./results/Bernoulli/Bernoulli_summer_07_iNEXT.png") %>% 
  magick::image_ggplot()
aut <- 
  magick::image_read("./results/Bernoulli/Bernoulli_autumn_07_iNEXT.png") %>% 
  magick::image_ggplot()
win <- 
  magick::image_read("./results/Bernoulli/Bernoulli_winter_07_iNEXT.png") %>% 
  magick::image_ggplot()
spr <- 
  magick::image_read("./results/Bernoulli/Bernoulli_spring_07_iNEXT.png") %>% 
  magick::image_ggplot()

ggsave(
  gridExtra::grid.arrange(
    (sum + ggtitle("Summer") + theme(plot.title = element_text(size = 7, face = "italic"))), 
    (aut + ggtitle("Autumn") + theme(plot.title = element_text(size = 7, face = "italic"))), 
    (win + ggtitle("Winter") + theme(plot.title = element_text(size = 7, face = "italic"))), 
    (spr + ggtitle("Spring") + theme(plot.title = element_text(size = 7, face = "italic"))),
    nrow = 4),
  filename = "./results/FigS8_iNEXT.png",
  height = 16, width = 10, units = "cm", dpi = 300
)

rm("sum", "aut", "win", "spr")
