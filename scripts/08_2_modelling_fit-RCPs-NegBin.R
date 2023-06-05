##
## ASG -- Region of Common Profile (NegBin)
## 
## Nicholas W Daudt
## ****************************************************************************#

## This code implements RCP models, 
## using count data based on 'Negative Binomial' error distribution.

## Libraries ####

library(plyr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(RColorBrewer)
library(rnaturalearth)
# library(devtools)
# devtools::install_github("skiptoniam/ecomix") # @dev
library(ecomix)

## Data ####

# RCP data
load("./data_out/rcp-data-season.rda")

# Env cols by season to model RCPs
load("./data_out/env-cols-to-model-season.rda")

# Helper functions for plotting
source("./scripts/source.R")

## Choose the number of groups -- run regional_mix.multifit ####

## General set up ------------------------------------------------------------ #

## Number of RCPs to fit
vec_rcps <- c(1:7)

# rcp_form will need to be specified inside the loop

## spp_form = sampling artifact formula
# As I'll fit each season separately, specify only Intercept
spp_form <- as.formula(~ 1) 

## Parameters to help 'grease the wheels' for model fitting 
## Based on Skip Woolley's EFI talk
# (https://github.com/eco4cast/Statistical-Methods-Seminar-Series/tree/main/woolley_ecomix)
control <- list(penalty = 0.01, penalty.tau = 10, penalty.gamma = 10,
                penalty.disp = c(10, sqrt(10)), quiet = TRUE)

## How many random starts should 'multifit' fit?
nstarts <- 100

## Loop through to get the results

seasons_vec <- c("summer", "autumn", "winter", "spring")

for (season_vec in seasons_vec) {
  print(season_vec)
  
  # Store results in a list
  nRCPs_samp <- list()
  
  # Read data
  rcp_data <- rcp_data_seasons[grepl(pattern = as.character(season_vec), 
                           x = names(rcp_data_seasons))]
  rcp_data <- rcp_data[[1]]
  
  env_cols <- names(rcp_data[, 5:17])
  
  # rcp_form = formula for fitting RCPs (i.e. "spp names ~ 1 + env vars")
  rcp_form <- paste0("cbind(", 
                     paste(names(rcp_data[, 18:ncol(rcp_data)]), collapse = ","),
                     ")~1+", 
                     paste(env_cols, collapse = "+"))
  
  ## Run 'regional_mix.multifit' --------------------------------------------- #
  for(i in vec_rcps) {
    
    nRCPs_samp[[i]] <- 
      ecomix::regional_mix.multifit(
        rcp_formula = rcp_form,
        species_formula = spp_form,
        data = rcp_data,
        nRCP = i,
        family = "negative.binomial",
        inits = "random2",
        control = control,
        nstart = nstarts)
    
    rm("i")
  }
  
  ## Get BIC values for each model ------------------------------------------- #
  RCPsamp_BICs <- sapply(nRCPs_samp, function(x) sapply(x, function(y) y$BIC))
  
  ## Get smaller BIC value for each RCP number ------------------------------- #
  RCPsamp_minBICs <- apply(RCPsamp_BICs, 2, min)
  
  ## Get T/F for <= N sites allocated for an RCP (at least 1 RCP) ------------ #
  L <- list()
  
  for (ii in 2:length(nRCPs_samp)) {
    
    x1 <- nRCPs_samp[[ii]]
    
    for (iii in 1:length(x1)) {
      
      x <- x1[[iii]]$postProbs
      L <- append(L, list(as.matrix(x)))
      
    }
    
    rm("ii", "iii", "x1", "x")
  }
  
  RCPsamp_BICs_less5 <- sapply(L, FUN = FUN_lessNsites, n = 5)
  RCPsamp_BICs_only1 <- sapply(L, FUN = FUN_lessNsites, n = 1)
  
  rm("L")
  
  ## Plot results ------------------------------------------------------------ #
  
  grps <- vec_rcps # needed for ggplot & df's below
  
  # data.frames for plotting
  df2a <- data.frame(grps = grps, bic = c(RCPsamp_minBICs))
  df2b <- data.frame(grps = rep(grps, each = 100), 
                     bic = c(as.numeric(unlist(RCPsamp_BICs))),
                     less_5 = as.factor(c(rep(NA, times = 100), RCPsamp_BICs_less5)), 
                     only_1 = as.factor(c(rep(NA, times = 100), RCPsamp_BICs_only1))) 
  
  df2b <- 
    df2b %>%
    tidyr::pivot_longer(cols = c("less_5", "only_1"),
                        names_to = "rcp_sites",
                        values_to = "value") %>%
    dplyr::mutate(rcp_sites = as.factor(rcp_sites))
  
  # Set colour palette
  pal <- c("#000000", brewer.pal(7, "YlOrRd")) # 0 == black
  
  # Set right labels for facets in the plot
  site_labs <- c("Five or less sites", "Only one site")
  names(site_labs) <- c("less_5", "only_1")
  
  # Plot BIC vs no. RCPs
  gg_multifitBIC <- 
    # plot min BIC
    ggplot(df2a[df2a$grps > 1,], aes(x = grps, y = bic)) +
    geom_point() + geom_line() +
    # plot the hundred models
    geom_point(data = df2b[df2b$grps > 1, ], 
               aes(x = grps, y = bic, colour = value)) +
    scale_x_continuous("Number of groups", 
                       labels = as.character(grps), breaks = grps) +
    scale_color_manual(values = pal,
                       name = "# RCPs") +
    facet_wrap(vars(rcp_sites), labeller = labeller(rcp_sites = site_labs)) +
    ylab("BIC") + 
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 8),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          legend.key.size = unit(0.5, "cm"))
  
  # Save plot
  ggsave(plot = gg_multifitBIC,
         filename = 
           paste0("./results/NegBin/NegBin_", as.character(season_vec), "_01_multifit-plot.png"),
         width = 13, height = 6, units = "cm", dpi = 300)
  
  ## Save results 'multifit' ------------------------------------------------- #
  save("nRCPs_samp",
       file = paste0("./results/NegBin/NegBin_", as.character(season_vec), "_01_multifit.rda"))
  
  # Also save objects used to generate the plot, just in case we need some adjustment
  save("grps", "df2a", "df2b", "site_labs",
       file = paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_01_multifit-plot-objs.rda"))
  
  ## Clean environment ------------------------------------------------------- #
  rm("nRCPs_samp", "rcp_data", "env_cols", "rcp_form", 
     "RCPsamp_BICs", "RCPsamp_minBICs", "RCPsamp_BICs_less5", "RCPsamp_BICs_only1", 
     "grps", "df2a", "df2b", "pal", "gg_multifitBIC", "site_labs", "season_vec")
  
  gc()
  
}
# > {tictoc}: ~26 min

rm("nstarts", "spp_form", "vec_rcps")

## Fix number of RCPs and run regional_mix ####

# # RCP data
# load("./data_out/rcp-data-season.rda")
# # Env cols by season to model RCPs
# load("./data_out/env-cols-to-model-season.rda")

## Set control parameters for another round of model fitting (using 'regional_mix');
## based on Skip Woolley's EFI talk
# (https://github.com/eco4cast/Statistical-Methods-Seminar-Series/tree/main/woolley_ecomix)
control <- list(optimise = TRUE, quiet = TRUE)
spp_form <- ~1

## Loop through to get see the results

# seasons_vec <- c("summer", "autumn", "winter", "spring")

for (season_vec in seasons_vec) {
  
  ## Read the data ------------------------------------------------------------- #
  rcp_data <- rcp_data_seasons[grepl(pattern = as.character(season_vec), 
                                     x = names(rcp_data_seasons))]
  rcp_data <- rcp_data[[1]]
  
  ## Get right environmental data columns to fit the models ------------------ #
  env_cols_models <- unlist(
    env_cols_models_season[grepl(pattern = as.character(season_vec), 
                                 x = names(env_cols_models_season))])
  
  ## Get all linear combination between the environmental variables ---------- #
  ## and create the formulas to run all the models
  
  # All linear combination between env vars
  models <- plyr::ldply(1:length(env_cols_models), 
                        function(x) t(combn(env_cols_models, x)))
  
  # Create formula-like strings, based on each of these combinations
  models <- tidyr::unite(data = models, 
                         col = env_vars, 
                         dplyr::everything(),
                         na.rm = TRUE, 
                         sep = "+")
  
  # Include species ('spp') and build model notation as per RCP specifications
  # (as in 'rcp_form' from 'multifit')
  models <- 
    models %>% 
    dplyr::mutate(spp = paste(names(rcp_data[, 18:ncol(rcp_data)]), collapse = ",")) %>%
    dplyr::mutate(model = paste0("cbind(", spp, ")~1+", env_vars))
  
  ## Create a list of models to loop through --------------------------------- #
  models_loop <- list()
  
  for (i in 1:nrow(models)) {
    x <- models[i, ]$model
    models_loop[[i]] <- x
    rm("x")
  }
  
  ## Index for the best model in 'multifit' ---------------------------------- #
  ## number according to best number of RCPs (**visually** inspected)
  
  nRCPbest <- 2
  
  ## Use the best model's parameters from 'multifit' as ---------------------- #
  ## starting values for 'regional_mix'
  
  # Map the files, and load the multimodels fit ('nRCPs_samp')
  multifit_files <- list.files(path = "./results/NegBin", pattern = "_01_multifit.rda$",
                               full.names = TRUE, recursive = TRUE)
  
  load(multifit_files[grepl(pattern = as.character(season_vec),
                            x = multifit_files)])
  
  # Get BIC values for each model -------------------------------------------- #
  RCPsamp_BICs <- sapply(nRCPs_samp, function(x) sapply(x, function(y) y$BIC))
  
  # Get best model index
  RCPsamp_bestmod <- which.min(RCPsamp_BICs[, nRCPbest])
  
  ## Run 'regional_mix' ------------------------------------------------------ #
  
  fit <- list()
  
  for (i in 1:length(models_loop)) {
    print(paste0("Model ", as.character(i), "/", as.character(length(models_loop))))
    
    model <- models_loop[[i]]
    
    # Name the model objects, to make it easier
    name <- sub('.', '',
                as.character(gsub("\\+", "_", sub(".*~1+", "", x = model))))
    
    fit[[name]] <-
      tryCatch({
        regional_mix(rcp_formula = model,
                     species_formula = spp_form,
                     nRCP = nRCPbest,
                     data = rcp_data,
                     family = "negative.binomial",
                     # Use initial values from the best model fit in 'multifit'
                     inits = unlist(nRCPs_samp[[nRCPbest]][[RCPsamp_bestmod]]$coef),
                     control = control)
      }, 
      error = function(e) { 
        message(e) 
        return(NULL) 
        }
      )
    
    rm("i")
  }
  
  save("fit", 
       file = paste0("./results/NegBin/NegBin_", as.character(season_vec), "_02_regional-mix.rda"))
  
  ## Clean environment
  rm("rcp_data", "env_cols_models", "models", "models_loop", 
     "nRCPbest", "multifit_files", "nRCPs_samp", "RCPsamp_BICs", "RCPsamp_bestmod", 
     "model", "name", "fit", "season_vec")
  gc()
}
# {tictoc}: ~35 min

rm("control", "spp_form")

## Choose the best model -- model selection based on BIC ####

# seasons_vec <- c("summer", "autumn", "winter", "spring")

for (season_vec in seasons_vec) {
  
  # Map the files, and load the multimodels fit ('fit')
  fit_files <- list.files(path = "./results/NegBin", pattern = "_02_regional-mix.rda$",
                               full.names = TRUE, recursive = TRUE)
  
  load(fit_files[grepl(pattern = as.character(season_vec),
                            x = fit_files)])
  
  ## Get "name" of the model (env vars) and respectives BICs
  fit_BICs <- data.frame(model = rep(NA, length(fit)),
                         bic = rep(NA, length(fit)))
  
  for (i in 1:length(fit)) {
    fit_BICs[i, 1] = names(fit[i])
    fit_BICs[i, 2] = fit[[i]]$BIC
    rm("i")
  }
  
  ## Min BIC
  BIC_min <- min(fit_BICs$bic)
  
  ## Prepare for plot
  fit_BICs <- 
    fit_BICs %>%
    dplyr::mutate(is_min_BIC = as.factor(ifelse(fit_BICs$bic == BIC_min, 'yes', 'no'))) %>%
    arrange(bic) %>%
    dplyr::mutate(delta_bic = bic - lag(bic)) %>%
    dplyr::mutate(delta_bic = (tidyr::replace_na(delta_bic, 0)))
  
  # ## All models --------------  probably not worth plotting this, too messy
  # plot_model_selection <- 
  #   ggplot(fit_BICs, aes(x = model, y = bic)) + 
  #   geom_point(aes(colour = is_min_BIC, shape = is_min_BIC)) +
  #   scale_shape_manual(values = c(16, 17)) +
  #   scale_color_manual(values = c('black','red')) +
  #   scale_size_manual(values = c(5, 5)) +
  #   xlab("") + ylab("BIC") + 
  #   theme_bw() + 
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
  #         legend.position = "none")
  
  ## Top 6 models
  plot_model_selection_topBIC <-
    ggplot(head(fit_BICs), aes(x = model, y = bic)) +
    geom_point(aes(colour = is_min_BIC, shape = is_min_BIC), size = 1.5) +
    scale_shape_manual(values = c(16, 17)) +
    scale_color_manual(values = c('black','red')) +
    # scale_size_manual(values = c(5, 5)) +
    xlab("") + ylab("BIC") +
    theme_bw() +
    theme(axis.title = element_text(size = 8),
          axis.text = element_text(size = 6),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none")
  
  ggsave(plot = plot_model_selection_topBIC,
         filename = 
           paste0("./results/NegBin/NegBin_", as.character(season_vec), "_03_model-selection-tops.png"),
         width = 7, height = 10, units = "cm", dpi = 300)
  
  ## Save 'best_model' for diagnostics
  
  # Get the name
  best_model_name <- fit_BICs[1,]$model

  # Get it from the 'fit' list
  best_model <- fit[[best_model_name]]

  # Save it
  save("best_model",
       file = paste0("./results/NegBin/NegBin_", as.character(season_vec), "_04_best-model.rda"))

  ## Clean environment
  rm("season_vec", "fit_files", "fit_BICs", "BIC_min",
     "plot_model_selection_topBIC", "fit",
     "best_model_name", "best_model")
  gc()
}

## Diagnostics ####

# seasons_vec <- c("summer", "autumn", "winter", "spring")

for (season_vec in seasons_vec) {
  
  print(season_vec)
  
  # Map the files, and load the best models for each season
  best_model_files <- list.files(path = "./results/NegBin", pattern = "_04_best-model.rda$",
                          full.names = TRUE, recursive = TRUE)
  
  load(best_model_files[grepl(pattern = as.character(season_vec),
                       x = best_model_files)])
  
  ## Model residuals --------------------------------------------------------- #
  png(filename = 
        paste0("./results/NegBin/NegBin_", as.character(season_vec), "_05_best-model-residuals.png"),
      width = 16, height = 8, units = "cm", res = 300)
  
  plot(best_model, type = "RQR", fitted.scale = "log")
  
  dev.off()
  
  ## RCPs stability ---------------------------------------------------------- #
  # How many sites to take out each round -- roughly 5, 10, and 20% of total sites
  if(season_vec == "summer") {
    oosSR <- c(2, 3, 5)
  } 
  if(season_vec == "autumn") {
    oosSR <- c(3, 5, 10)
  } 
  if(season_vec == "winter") {
    oosSR <- c(4, 6, 11)
  } 
  if(season_vec == "spring") {
    oosSR <- c(5, 10, 20)
  }
  
  best_model_stab <- 
    ecomix::stability.regional_mix(best_model, 
                                   oosSizeRange = oosSR,
                                   mc.cores = 1,
                                   doPlot = FALSE)
  
  save("best_model_stab", 
       file = paste0("./results/NegBin/NegBin_", as.character(season_vec), "_05_model-stability.rda"))
  
  ## Plot stability
  # 1) leave-some-out Cook's distance against holdout sample size
  # 2) the predictive log-likelihood for times sites, against the holdout sample size
  
  png(filename = 
        paste0("./results/NegBin/NegBin_", as.character(season_vec), "_05_best-model-stability.png"),
      width = 16, height = 8, units = "cm", res = 300)
  
  par(mfrow = c(1, 2))
  plot(best_model_stab, minWidth = 2)
  
  dev.off()
  
  rm("best_model_files", "best_model", "best_model_stab", "oosSR", "season_vec")
  gc()
}

## Bootstrap 'best_model' ####

# seasons_vec <- c("summer", "autumn", "winter", "spring")

for (season_vec in seasons_vec) {
  
  print(season_vec)
  
  # Map the files, and load the best models for each season
  best_model_files <- list.files(path = "./results/NegBin", pattern = "_04_best-model.rda$",
                                 full.names = TRUE, recursive = TRUE)
  
  load(best_model_files[grepl(pattern = as.character(season_vec),
                              x = best_model_files)])
  
  ## Bootstrap & save it
  best_model_boot <- ecomix:::regional_mix.bootstrap(best_model)
  
  save("best_model_boot",
       file = 
         paste0("./results/NegBin/NegBin_", as.character(season_vec), "_06_best-model-bootstrap.rda"))
  
  rm("best_model_files", "best_model", "best_model_boot", "season_vec")
  gc()
}

## Species profiles ####

# seasons_vec <- c("summer", "autumn", "winter", "spring")

for (season_vec in seasons_vec) {
  
  ## Map the files, and load the best models and their bootstraps, for each season
  best_model_files <- list.files(path = "./results/NegBin", pattern = "_04_best-model.rda$",
                                 full.names = TRUE, recursive = TRUE)
  
  load(best_model_files[grepl(pattern = as.character(season_vec),
                              x = best_model_files)])
  
  best_model_boot_files <- list.files(path = "./results/NegBin", pattern = "_06_best-model-bootstrap.rda$",
                                 full.names = TRUE, recursive = TRUE)
  
  load(best_model_boot_files[grepl(pattern = as.character(season_vec),
                                   x = best_model_boot_files)])
  
  
  ## Average + CI (from bootstrap) species profiles per RCP
  best_model_spp_profile_boot <- ecomix::regional_mix.species_profile(best_model,
                                                                      best_model_boot)
  
  ## Average probability
  spp_profile_boot_mean <- 
    as.data.frame(unlist(best_model_spp_profile_boot$overall$mean)) %>% 
    tibble::rownames_to_column(., var = "rcp") %>% 
    tidyr::pivot_longer(cols = !rcp,
                        names_to = "species",
                        values_to = "mean") %>%
    dplyr::mutate(mean = round(mean, digits = 2))
  
  ## Lower CI interval
  spp_profile_boot_lower <- 
    as.data.frame(unlist(best_model_spp_profile_boot$overall$lower)) %>% 
    tibble::rownames_to_column(., var = "rcp") %>% 
    tidyr::pivot_longer(cols = !rcp,
                        names_to = "species",
                        values_to = "lowerCI") %>%
    dplyr::mutate(lowerCI = round(lowerCI, digits = 2))
  
  ## Upper CI interval
  spp_profile_boot_upper <- 
    as.data.frame(unlist(best_model_spp_profile_boot$overall$upper)) %>% 
    tibble::rownames_to_column(., var = "rcp") %>% 
    tidyr::pivot_longer(cols = !rcp,
                        names_to = "species",
                        values_to = "upperCI") %>%
    dplyr::mutate(upperCI = round(upperCI, digits = 2))
  
  spp_profile_boot <- 
    cbind(spp_profile_boot_mean,
          lowerCI = spp_profile_boot_lower$lowerCI,
          upperCI = spp_profile_boot_upper$upperCI)
  
  spp_profile_boot$rcp <- factor(spp_profile_boot$rcp, levels = c("RCP1",
                                                                  "RCP2"))
  
  rm("spp_profile_boot_mean", "spp_profile_boot_lower", "spp_profile_boot_upper")
  
  ## Plot -------------------------------------------------------------------- #
  plot_spp_profiles_CI <- 
    ggplot(spp_profile_boot,
           aes(x = log(mean), y = species, group = rcp, color = rcp)) + 
    geom_point() +
    geom_errorbarh(aes(xmin = log(lowerCI), xmax = log(upperCI))) +
    scale_color_brewer(palette = "Dark2") +
    facet_wrap(vars(rcp), ncol = 3) + 
    ylab("") + xlab("Log(predicted mean abundance) + Confidence Interval") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 7),
          strip.text = element_text(size = 8))
  
  ggsave(plot = plot_spp_profiles_CI, 
         filename = 
           paste0("./results/NegBin/NegBin_", as.character(season_vec), "_07_species-profiles.png"),
         width = 16, height = 8, units = "cm", dpi = 300)
  
  ## Clean environment
  rm("best_model_files", "best_model_boot_files", "season_vec",
     "best_model", "best_model_boot", "best_model_spp_profile_boot",
     "spp_profile_boot", "plot_spp_profiles_CI")
  gc()
  
}

## Predict and map: Point prediction ####

# seasons_vec <- c("summer", "autumn", "winter", "spring")

## Australia spatial polygon & 'basemap'
aus_sf <- rnaturalearth::ne_countries(country = "australia", returnclass = "sf")

aus_base_map <- 
  ggplot(data = aus_sf) + 
  geom_sf(color = "black", fill = "lightgrey") + 
  coord_sf(xlim = c(140, 161), ylim = c(-9, -48)) + 
  theme_bw()

## Grids -- select grid 1x1 degree
load("./data_out/grids_0.5-2.rda")
g1 <- grids[["g1"]]; rm("grids")

## Point prediction ---------------------------------------------------------- #
plotRCPs_all <- data.frame()

for (season_vec in seasons_vec) {
  
  ## Read RCP data ----------------------------------------------------------- #
  # load("./data_out/rcp-data-season.rda")
  rcp_data <- rcp_data_seasons[grepl(pattern = as.character(season_vec), 
                                     x = names(rcp_data_seasons))]
  rcp_data <- rcp_data[[1]]
  
  ## Read best_model --------------------------------------------------------- #
  best_model_files <- list.files(path = "./results/NegBin", pattern = "_04_best-model.rda$",
                                 full.names = TRUE, recursive = TRUE)
  
  load(best_model_files[grepl(pattern = as.character(season_vec),
                              x = best_model_files)])
  
  ## Get point predictions --------------------------------------------------- #
  pred <- predict(best_model)
  
  rcp_select = c("RCP_1", "RCP_2")
  
  plotRCPs <- 
    as.data.frame(pred) %>% 
    dplyr::select(all_of(rcp_select)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(final_RCP = 
                    factor(
                      names(.)[which.max(dplyr::c_across(cols = everything()))]))
  
  ## Bind with rcp_data for getting 'IDgrid' & merge with 'g1'
  plotRCPs <- 
    cbind(rcp_data, plotRCPs)
  
  plotRCPs <- merge(g1, plotRCPs, by = "IDgrid")
  
  # Quick tidy-up to plot it right
  plotRCPs$final_RCP <- gsub(pattern = "_", replacement = "", x = plotRCPs$final_RCP)
  plotRCPs$final_RCP <- factor(plotRCPs$final_RCP, levels = c("RCP1", "RCP2"))
  
  plotRCPs <-
    plotRCPs %>%
    dplyr::select(IDgrid, season, final_RCP, geometry)
  
  plotRCPs_all <- rbind(plotRCPs_all, plotRCPs)
  
  ## Clean environment
  rm("rcp_data", "best_model_files", "season_vec",
     "best_model", "pred", "rcp_select", "plotRCPs")
  gc()
  
}

plotRCPs_all$season <- factor(plotRCPs_all$season, levels = c("summer", "autumn",
                                                              "winter", "spring"))

## Map it -------------------------------------------------------------------- #
map_point_pred <-
  aus_base_map + 
  geom_sf(data = plotRCPs_all, aes(fill = final_RCP)) + 
  coord_sf(xlim = c(140, 161), ylim = c(-9, -48)) +
  scale_x_continuous(breaks = c(140, 150, 160)) +
  scale_fill_brewer(palette = "Dark2", name = "Hard-class RCP") + 
  facet_wrap(~season, ncol = 4) + 
  theme(legend.position = "right",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7.5),
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 6))

ggsave(plot = map_point_pred, 
       filename = 
         paste0("./results/NegBin/NegBin_07_RCP-point-predictions.png"),
       height = 10, width = 20, units = "cm", dpi = 300)

rm("map_point_pred")

## Predict and map: Uncertainty ####

# seasons_vec <- c("summer", "autumn", "winter", "spring")

# ## Australia spatial polygon
# aus_sf <- rnaturalearth::ne_countries(country = "australia", returnclass = "sf")
# 
# aus_base_map <-
#   ggplot(data = aus_sf) +
#   geom_sf(color = "black", fill = "lightgrey") +
#   coord_sf(xlim = c(140, 161), ylim = c(-9, -48)) +
#   theme_bw()
# 
# ## Grids -- select grid 1x1 degree
# load("./data_out/grids_0.5-2.rda")
# g1 <- grids[["g1"]]; rm("grids")

## Uncertainty: mean + CI probabilities of RCPs per grid --------------------- #

for (season_vec in seasons_vec) {
  
  # ## Read RCP data ----------------------------------------------------------- #
  # load("./data_out/inputs/rcp-data-season.rda")
  # rcp_data <- rcp_data_seasons[grepl(pattern = as.character(season_vec), 
  #                                    x = names(rcp_data_seasons))]
  # rcp_data <- rcp_data[[1]]
  
  ## Read best_model --------------------------------------------------------- #
  best_model_files <- list.files(path = "./results/NegBin", pattern = "_04_best-model.rda$",
                                 full.names = TRUE, recursive = TRUE)
  
  load(best_model_files[grepl(pattern = as.character(season_vec),
                              x = best_model_files)])
  
  ## Read bootstrap from best_model ------------------------------------------ #
  best_model_boot_files <- list.files(path = "./results/NegBin", 
                                      pattern = "_06_best-model-bootstrap.rda$",
                                      full.names = TRUE, recursive = TRUE)
  
  load(best_model_boot_files[grepl(pattern = as.character(season_vec),
                                   x = best_model_boot_files)])
  
  ## Read environmental data from prediction grid ------------------------------ #
  load("./data_out/pred-data-seasons.rda")
  pred_grid_data <- pred_data_seasons[grepl(pattern = as.character(season_vec), 
                                            x = names(pred_data_seasons))]
  pred_grid_data <- pred_grid_data[[1]]
  pred_grid_data <- na.omit(pred_grid_data)
  
  ## Predict to the grid ----------------------------------------------------- #
  pred <- predict(object = best_model,
                  object2 = best_model_boot,
                  newdata = pred_grid_data)
  
  predRCP <- as.data.frame(unlist(pred$bootPreds))
  predCI <- as.data.frame(unlist(pred$bootCIs))
  
  pred_data <- cbind(pred_grid_data[, 1:3], predRCP, predCI)
  pred_data <- merge(g1, pred_data, by = "IDgrid")
  
  rcp_map = c("RCP_1", "RCP_2")
  
  plotRCPs <- FUN_prob_maps(column_names = rcp_map)
  
  ggsave(plot = plotRCPs, 
         filename = 
           paste0("./results/NegBin/NegBin_", as.character(season_vec), "_07_RCP-prob-predictions.png"),
         width = 11, height = 11, units = "cm", dpi = 300)
  
  ## Clean environment
  rm("best_model_files", "best_model",
     "best_model_boot_files", "best_model_boot",
     "season_vec",
     "pred_grid_data", "pred", "predRCP", "predCI", "pred_data",
     "rcp_map", "plotRCPs")
  gc()
  
}

## RCPs ~ env. data [COME BACK HERE AND RUN + TIDY IT UP] ####

# seasons_vec <- c("summer", "autumn", "winter", "spring")

## To store data for plot
plot_data <- data.frame()

## Best model formulas
best_model_forms <- list()

## Get data for plots
for (season_vec in seasons_vec) {
  
  ## Read RCP data ----------------------------------------------------------- #
  # load("./data_out/rcp-data-season.rda")
  rcp_data <- rcp_data_seasons[grepl(pattern = as.character(season_vec),
                                     x = names(rcp_data_seasons))]
  rcp_data <- rcp_data[[1]]
  
  env_vars <- colnames(rcp_data[5:17])
  
  ## Read best_model --------------------------------------------------------- #
  best_model_files <- list.files(path = "./results/NegBin", pattern = "_04_best-model.rda$",
                                 full.names = TRUE, recursive = TRUE)
  
  load(best_model_files[grepl(pattern = as.character(season_vec),
                              x = best_model_files)])
  
  # Get env var(s) name(s) from the selected best model
  best_model_vars <- as.character(attr(best_model$terms$xterms, which = "variables"))
  best_model_vars <- best_model_vars[2:length(best_model_vars)]
  
  name <- as.character(season_vec)
  best_model_forms[[name]] <- best_model_vars
  
  ## Get point-predictions for RCPs ------------------------------------------ #
  pred <- predict(best_model)
  
  ## Read data with original environmental data values 
  df <- read.csv("./data_out/seabirds_rcp.csv")
  
  df <- 
    df %>%
    dplyr::filter(season == season_vec) %>%
    dplyr::semi_join(., rcp_data[1], by = "IDgrid") %>%
    dplyr::select(IDgrid, season, all_of(env_vars)) %>%
    cbind(., as.data.frame(pred))
  
  ## Return and bind data
  plot_data <- rbind(plot_data, df)
  
  rm("rcp_data_seasons", "rcp_data", "env_vars", "best_model_files", "best_model", 
     "best_model_vars", "name", "pred", "df", "season_vec")
  gc()
  
}

## Plot RCP~env.data
for (season_vec in seasons_vec) {
  
  vars <- best_model_forms[[season_vec]]
  
  df <- 
    plot_data %>%
    dplyr::select(contains("RCP"), all_of(vars))
  
  df <- dplyr::select(df, - RCP_3)
  df <- 
    df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(final_RCP = 
    factor(
      names(.)[which.max(dplyr::c_across(cols = starts_with("RCP_")))],
      levels = c(names(df[1]), names(df[2]))))
  df$max_value_RCP <- do.call(`pmax`, df[1:2])
  df <- dplyr::select(df, -starts_with("RCP_"))
  
  df <- 
    df %>%
    tidyr::pivot_longer(cols = all_of(vars),
                        names_to = "env_vars",
                        values_to = "value_env")
  
  ### ENVIRONMENTAL DATA LABELS ---------------------------------------- ###
  # (...)
  
  plot <- 
    ggplot(df, aes(x = value_env, y = max_value_RCP, color = final_RCP)) + 
    geom_point(size = 1.5, alpha = 0.7) + 
    scale_color_brewer(palette = "Dark2") +
    facet_wrap(~env_vars, scales = "free_x") + 
    geom_smooth(se = FALSE) + 
    # facet_grid(rows = vars(rcps), cols = vars(env_vars), scales = "free_x") + 
    ylab("RCP membership probability") + xlab("Environmental gradient") +
    theme_bw() + 
    theme(legend.position = "none",
          strip.text = element_text(face = "bold"))
  
  ggsave(plot = plot, 
         filename = 
           paste0("./results/NegBin/NegBin_", as.character(season_vec), "_RCP-env-data.png"),
         #width = 12, height = 12, units = "cm", 
         dpi = 300)
  
  rm("vars", "df", "plot", "season_vec")
}

