##
## ASG -- Region of Common Profile (Bernoulli)
## 
## Nicholas W Daudt
## ****************************************************************************#

## This code implements RCP models, 
## using presence/absence data based on 'Bernoulli' as error distribution.

## Libraries ####

library(plyr)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(rnaturalearth)
# library(devtools)
# devtools::install_github("skiptoniam/ecomix") # @dev
library(ecomix)
library(iNEXT)
library(patchwork)

## Data ####

# RCP data
load("./data_out/rcp-data-season.rda")

# Env cols by season to model RCPs
load("./data_out/env-cols-to-model-season.rda")

# Helper functions for plots
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
## Based on Skip Woolley EFI's talk
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
  
  # Transform the data into presence/absence [1/0]
  spp_cols <- names(rcp_data[, 18:ncol(rcp_data)])
  
  rcp_data[spp_cols] <- lapply(rcp_data[spp_cols], 
                                function(x) replace(x, x >= 1, 1))
  
  # Get environmental variables names
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
        family = "bernoulli",
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
    # How many RCPs with <= 5 or 1 sites allocated?
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
  
  ## Save plot
  ggsave(plot = gg_multifitBIC,
         filename = 
           paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_01_multifit-plot.png"),
         width = 13, height = 6, units = "cm", dpi = 300)
  
  save("gg_multifitBIC",
       file = paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_01_multifit-plot-ggplot.rda"))
  
  ## Save results 'multifit' ------------------------------------------------- #
  save("nRCPs_samp",
       file = paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_01_multifit.rda"))
  
  # Also save objects used to generate the plot, just in case we need some adjustment
  save("grps", "df2a", "df2b", "site_labs",
       file = paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_01_multifit-plot-objs.rda"))
  
  ## Clean environment ------------------------------------------------------- #
  rm("nRCPs_samp", "rcp_data", "env_cols", "rcp_form", 
     "RCPsamp_BICs", "RCPsamp_minBICs", "RCPsamp_BICs_less5", "RCPsamp_BICs_only1", 
     "grps", "df2a", "df2b", "pal", "gg_multifitBIC", "site_labs", "season_vec")
  
  gc()
  
}
# > {tictoc}: ~10 min

rm("nstarts", "spp_form", "vec_rcps", "control")

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
  
  # Transform the data into presence/absence [1/0]
  spp_cols <- names(rcp_data[, 18:ncol(rcp_data)])
  
  rcp_data[spp_cols] <- lapply(rcp_data[spp_cols], 
                               function(x) replace(x, x >= 1, 1))
  
  ## Get right environmental data columns to fit the models ------------------ #
  env_cols_models <- unlist(
    env_cols_models_season[grepl(pattern = as.character(season_vec), 
                                 x = names(env_cols_models_season))])
  
  ## Get all additive, linear combination between the env vars --------------- #
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
  
  if(season_vec == "spring") {
    nRCPbest <- 2
  }
  if(season_vec == "autumn") {
    nRCPbest <- 3
  }
  if(season_vec == "summer") {
    nRCPbest <- 2
  }
  if(season_vec == "winter") {
    nRCPbest <- 2
  }
  
  ## Use the best model's parameters from 'multifit' as ---------------------- #
  ## starting values for 'regional_mix'
  
  # Map the files, and load the multimodels fit ('nRCPs_samp')
  multifit_files <- list.files(path = "./results/Bernoulli", pattern = "_01_multifit.rda$",
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
                     family = "bernoulli",
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
       file = paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_02_regional-mix.rda"))
  
  ## Clean environment
  rm("rcp_data", "env_cols_models", "models", "models_loop", 
     "nRCPbest", "multifit_files", "nRCPs_samp", "RCPsamp_BICs", "RCPsamp_bestmod", 
     "model", "name", "fit", "season_vec")
  gc()
}
# > {tictoc}: ~6 min

rm("control", "spp_form")

## Choose the best model -- model selection based on BIC ####

# seasons_vec <- c("summer", "autumn", "winter", "spring")

for (season_vec in seasons_vec) {
  
  # Map the files, and load the multimodels fit ('fit')
  fit_files <- list.files(path = "./results/Bernoulli", pattern = "_02_regional-mix.rda$",
                          full.names = TRUE, recursive = TRUE)
  
  load(fit_files[grepl(pattern = as.character(season_vec),
                       x = fit_files)])
  
  ## Get "name" of the model (env vars) and respective BICs
  fit_BICs <- data.frame(model = rep(NA, length(fit)),
                         bic = rep(NA, length(fit)))
  
  for (i in 1:length(fit)) {
    fit_BICs[i, 1] = names(fit[i])
    fit_BICs[i, 2] = fit[[i]]$BIC
    rm("i")
  }
  
  fit_BICs <- fit_BICs %>% dplyr::filter(!is.infinite(bic))
  
  ## Min BIC
  BIC_min <- min(fit_BICs$bic)
  
  ## Prepare for plot
  fit_BICs <- 
    fit_BICs %>%
    dplyr::mutate(is_min_BIC = as.factor(ifelse(fit_BICs$bic == BIC_min, 'yes', 'no'))) %>%
    dplyr::arrange(bic) %>%
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
    scale_color_manual(values = c("black", "red")) +
    # scale_size_manual(values = c(5, 5)) +
    xlab("") + ylab("BIC") +
    theme_bw() +
    theme(axis.title = element_text(size = 8),
          axis.text = element_text(size = 6),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none")
  
  ggsave(plot = plot_model_selection_topBIC,
         filename = 
           paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_03_model-selection-tops.png"),
         width = 7, height = 10, units = "cm", dpi = 300)
  
  ## Save 'best_model' for diagnostics
  
  # Get the best model name
  best_model_name <- fit_BICs[1,]$model
  
  # Get it from the 'fit' list
  best_model <- fit[[best_model_name]]
  
  # Just a tweak on best_model_name, for later on
  best_model_name_form <- 
    gsub(best_model_name, pattern = "_mean_", replacement = " + ") %>% 
    stringr::str_sub(., end = -6)
  
  # Save the best model object
  save("best_model", 
       file = paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_04_best-model.rda"))
  
  ### Get the 'full model name & full/best model infos
  full_model_name_form <- 
    paste(
      # First, just 'fix' variable names, as all end with "_mean" that doesn't need to be there
      lapply(env_cols_models_season, gsub, pattern = "_mean", replacement = "")[[season_vec]],
      # Then, collapse
      collapse = " + ") %>% 
    gsub(., pattern = "clim_eke", replacement = "clim_eke_mean")
  
  # To get full model' BIC
  full_model_name <- paste(env_cols_models_season[[season_vec]], collapse = "_")
  
  full_and_best_models <- data.frame(
    data_type = rep("bernoulli", times = 2),
    season = rep(season_vec, times = 2),
    model = c("full",
              "best"),
    model_specification = c(full_model_name_form,
                            best_model_name_form),
    bic = c(fit_BICs[fit_BICs$model==full_model_name,]$bic,
            fit_BICs[fit_BICs$model==best_model_name,]$bic)
  )
  
  write.csv(full_and_best_models, 
            file = paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_04_full_and_best_models.csv"))
  
  ## Clean environment
  rm("season_vec", "fit_files", "fit_BICs", "BIC_min",
     "plot_model_selection_topBIC", "fit",
     "best_model_name", "best_model_name_form", "best_model",
     "full_model_name", "full_model_name_form", "full_and_best_models")
  gc()
}

## Diagnostics ####

# seasons_vec <- c("summer", "autumn", "winter", "spring")

for (season_vec in seasons_vec) {
  
  print(season_vec)
  
  # Map the files, and load the best models for each season
  best_model_files <- list.files(path = "./results/Bernoulli", pattern = "_04_best-model.rda$",
                                 full.names = TRUE, recursive = TRUE)
  
  load(best_model_files[grepl(pattern = as.character(season_vec),
                              x = best_model_files)])
  
  ## Model residuals --------------------------------------------------------- #
  png(filename = 
        paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_05_best-model-residuals.png"),
      width = 16, height = 8, units = "cm", res = 300)
  
  plot(best_model, type = "RQR", fitted.scale = "log")
  
  dev.off()
  
  ## RCPs stability ---------------------------------------------------------- #
  # How many sites to take out each round -- 
  # these numbers correspond roughly to 5%, 10%, and 20% of total sites
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
       file = paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_05_model-stability.rda"))
  
  ## Plot stability
  # 1) leave-some-out Cook's distance against holdout sample size
  # 2) the predictive log-likelihood for times sites, against the holdout sample size
  
  png(filename = 
        paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_05_best-model-stability.png"),
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
  best_model_files <- list.files(path = "./results/Bernoulli", pattern = "_04_best-model.rda$",
                                 full.names = TRUE, recursive = TRUE)
  
  load(best_model_files[grepl(pattern = as.character(season_vec),
                              x = best_model_files)])
  
  ## Bootstrap & save it
  best_model_boot <- ecomix:::regional_mix.bootstrap(best_model)
  
  save("best_model_boot",
       file = 
         paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_06_best-model-bootstrap.rda"))
  
  rm("best_model_files", "best_model", "best_model_boot", "season_vec")
  gc()
}

## Species profiles ####

# seasons_vec <- c("summer", "autumn", "winter", "spring")

for (season_vec in seasons_vec) {
  
  ## Map the files, and load the best models and their bootstraps, for each season
  best_model_files <- list.files(path = "./results/Bernoulli", pattern = "_04_best-model.rda$",
                                 full.names = TRUE, recursive = TRUE)
  
  load(best_model_files[grepl(pattern = as.character(season_vec),
                              x = best_model_files)])
  
  best_model_boot_files <- list.files(path = "./results/Bernoulli", pattern = "_06_best-model-bootstrap.rda$",
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
    tidyr::pivot_longer(cols = ! rcp,
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
  
  rm("spp_profile_boot_mean", "spp_profile_boot_lower", "spp_profile_boot_upper")
  
  spp_profile_boot$rcp <- factor(spp_profile_boot$rcp, levels = c("RCP1",
                                                                  "RCP2",
                                                                  "RCP3"))
  
  ## Plot -------------------------------------------------------------------- #
  plot_spp_profiles_CI <- 
    ggplot(spp_profile_boot,
           aes(x = mean, y = species, group = rcp, color = rcp)) + 
    geom_point() +
    geom_errorbarh(aes(xmin = lowerCI, xmax = upperCI)) +
    scale_color_brewer(palette = "Dark2") +
    facet_wrap(vars(rcp), ncol = 3) + 
    ylab("") + xlab("Probability of Occurrence + Confidence Interval") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 7),
          strip.text = element_text(size = 8))
  
  ggsave(plot = plot_spp_profiles_CI, 
         filename = 
           paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_07_species-profiles.png"),
         width = 16, height = 8, units = "cm", dpi = 300)
  
  # Save ggplot objects
  save("plot_spp_profiles_CI",
       file = paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_07_species-profiles-ggplot.rda"))
  
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
  load("./data_out/rcp-data-season.rda")
  rcp_data <- rcp_data_seasons[grepl(pattern = as.character(season_vec), 
                                     x = names(rcp_data_seasons))]
  rcp_data <- rcp_data[[1]]
  
  ## Read best_model --------------------------------------------------------- #
  best_model_files <- list.files(path = "./results/Bernoulli", pattern = "_04_best-model.rda$",
                                 full.names = TRUE, recursive = TRUE)
  
  load(best_model_files[grepl(pattern = as.character(season_vec),
                              x = best_model_files)])
  
  ## Get point predictions --------------------------------------------------- #
  pred <- predict(best_model)
  
  if(season_vec == "spring"){
    rcp_select = c("RCP_1", "RCP_2")
  }
  if(season_vec == "autumn"){
    rcp_select = c("RCP_1", "RCP_2", "RCP_3")
  }
  if(season_vec == "summer"){
    rcp_select = c("RCP_1", "RCP_2")
  }
  if(season_vec == "winter"){
    rcp_select = c("RCP_1", "RCP_2")
  }
  
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
  plotRCPs$final_RCP <- factor(plotRCPs$final_RCP, levels = c("RCP1", "RCP2", "RCP3"))
  
  plotRCPs <-
    plotRCPs %>%
    dplyr::select(IDgrid, season, final_RCP, geometry)
  
  plotRCPs_all <- rbind(plotRCPs_all, plotRCPs)
  
  ## Clean environment
  rm("rcp_data", "best_model_files", "season_vec",
     "best_model", "pred", "rcp_select", "plotRCPs")
     # "map_point_pred")
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
       filename = "./results/Bernoulli/Bernoulli_07_RCP-point-predictions.png",
       height = 10, width = 20, units = "cm", dpi = 300)

save("map_point_pred",
     file = "./results/Bernoulli/Bernoulli_07_RCP-point-predictions-ggplot.rda")

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
  best_model_files <- list.files(path = "./results/Bernoulli", pattern = "_04_best-model.rda$",
                                 full.names = TRUE, recursive = TRUE)
  
  load(best_model_files[grepl(pattern = as.character(season_vec),
                              x = best_model_files)])
  
  ## Read bootstrap from best_model ------------------------------------------ #
  best_model_boot_files <- list.files(path = "./results/Bernoulli", 
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
  
  if(season_vec == "autumn"){
    rcp_map = c("RCP_1", "RCP_2", "RCP_3")
  } else {
    rcp_map = c("RCP_1", "RCP_2")
  }
  
  if(season_vec == "summer") {
    
    # As RCP group colours are being depicted from 'south' (RCP1) to 'north' (RCP2),
    # we had to do a hack here for the probability maps visually agree with 
    # point-predictions in a meaningful way (basically, swap "RCP1" to "RCP2", and vice-versa)
    
    new_col_names <- c("IDgrid", "lon", "lat", 
                       "RCP_2", "RCP_1", 
                       "RCP_2.lower", "RCP_1.lower", 
                       "RCP_2.upper", "RCP_1.upper", 
                       "geometry")
    
    colnames(pred_data) <- new_col_names
    
    rm("new_col_names")
  }
  
  plotRCPs <- FUN_prob_maps(column_names = rcp_map)
  
  save("plotRCPs",
       file = paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_07_RCP-prob-predictions-ggplot.rda"))
  
  if(season_vec == "autumn"){
    ggsave(plot = plotRCPs, 
           filename = 
             paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_07_RCP-prob-predictions.png"),
           width = 11, height = 14, units = "cm", dpi = 300)
  } else {
    ggsave(plot = plotRCPs, 
           filename = 
             paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_07_RCP-prob-predictions.png"),
           width = 11, height = 11, units = "cm", dpi = 300)
  }
  
  ## Clean environment
  rm("best_model_files", "best_model",
     "best_model_boot_files", "best_model_boot",
     "season_vec",
     "pred_grid_data", "pred", "predRCP", "predCI", "pred_data",
     "rcp_map", "plotRCPs")
  gc()
  
}

## RCPs ~ env. data ####

load("./data_out/attributes-scaled-env-data-season.rda")

# seasons_vec <- c("summer", "autumn", "winter", "spring")

for (season_vec in seasons_vec) {
  
  ### Get scale parameters to plot predictors in their original values ------- #
  scale_params <- 
    env_data_seasons[stringr::str_detect(names(env_data_seasons), as.character(season_vec))]
  
  scale_params <- scale_params[[1]]
  
  mean <- 
    as.data.frame(attr(scale_params, "scaled:center")) %>% 
    tibble::rownames_to_column() %>% 
    dplyr::rename(scaled_center = "attr(scale_params, \"scaled:center\")")
  
  sd <- 
    as.data.frame(attr(scale_params, "scaled:scale")) %>% 
    tibble::rownames_to_column() %>% 
    dplyr::rename(scaled_center = "attr(scale_params, \"scaled:scale\")")
  
  rm("scale_params")
  
  ### Get 'beta.mat' --------------------------------------------------------- #
  # Read best_model
  best_model_files <- list.files(path = "./results/Bernoulli", pattern = "_04_best-model.rda$",
                                 full.names = TRUE, recursive = TRUE)
  
  load(best_model_files[grepl(pattern = as.character(season_vec),
                              x = best_model_files)])
  
  rm("best_model_files")
  
  beta.mat <- coef(best_model)$beta
  
  pred_names <- colnames(beta.mat)[2:ncol(beta.mat)]
  
  ### To set 'xlm.real' get 'original' data ---------------------------------- #
  original_env_values <- 
    read.csv("./data_out/seabirds_rcp.csv")
  
  original_env_values <-
    original_env_values %>% 
    dplyr::filter(season == as.character(season_vec)) %>% 
    dplyr::select(all_of(pred_names))
  
  ### Partial plot(s) -------------------------------------------------------- #
  
  # Set 'predplot' vectors
  predplot <- list()
  
  for (i in 1:length(pred_names)) {
    plot_vec <- rep(0,length(pred_names))
    plot_vec[i] <- NA
    predplot[[i]] <- plot_vec
    rm("plot_vec", "i")
  }
  
  # Set colour palette
  if(best_model$nRCP == 2){
    coluse_pal <- c("#1b9e77", "#d95f02")
  } else {
    coluse_pal <- c("#1b9e77", "#d95f02", "#7570b3")
  }
  
  for (i in 1:length(pred_names)) {
    
    name <- pred_names[i]
    
    png(filename = paste0("./results/Bernoulli/Bernoulli_", 
                          as.character(season_vec), 
                          "_07_partial-plot-",
                          gsub(stringr::str_sub(name, end = -6),
                               pattern = "_", replacement = "-"),
                          ".png"),
        width = 12, height = 12, units = "cm", res = 300)
    
    FUN_partial_plot(beta.mat = beta.mat, 
                     predplot = predplot[[i]], 
                     xlm.real = c(
                       min(original_env_values[,name], na.rm = TRUE),
                       max(original_env_values[,name], na.rm = TRUE)
                     ), 
                     stand.vals = c(
                       mean[mean$rowname == name, 2],
                       sd[sd$rowname == name, 2]
                     ), 
                     n = 1001, 
                     xlb = stringr::str_sub(name, end = -6), 
                     ylb = "Probability", 
                     location = "right", 
                     coluse = coluse_pal)
    dev.off()
    
    rm("name", "i")
  }
  
  rm("mean", "sd", 
     "best_model", "beta.mat", "pred_names", 
     "original_env_values", 
     "predplot", "coluse_pal",
     "season_vec")
  
}

rm("env_data_seasons")

## iNEXT ####

# seasons_vec <- c("summer", "autumn", "winter", "spring")

for (season_vec in seasons_vec) {
  
  ## Read RCP data ----------------------------------------------------------- #
  load("./data_out/rcp-data-season.rda")
  rcp_data <- rcp_data_seasons[grepl(pattern = as.character(season_vec), 
                                     x = names(rcp_data_seasons))]
  rcp_data <- rcp_data[[1]]
  
  # Transform the data into presence/absence [1/0]
  spp_cols <- names(rcp_data[, 18:ncol(rcp_data)])
  
  rcp_data[spp_cols] <- lapply(rcp_data[spp_cols], 
                               function(x) replace(x, x >= 1, 1))
  
  ## Read best_model --------------------------------------------------------- #
  best_model_files <- list.files(path = "./results/Bernoulli", pattern = "_04_best-model.rda$",
                                 full.names = TRUE, recursive = TRUE)
  
  load(best_model_files[grepl(pattern = as.character(season_vec),
                              x = best_model_files)])
  
  ## Get point predictions --------------------------------------------------- #
  pred <- predict(best_model)
  
  if(season_vec == "autumn"){
    rcp_select = c("RCP_1", "RCP_2", "RCP_3")
  } else {
    rcp_select = c("RCP_1", "RCP_2")
  }
  
  finalRCP <- 
    as.data.frame(pred) %>% 
    dplyr::select(all_of(rcp_select)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(final_RCP = 
                    factor(
                      names(.)[which.max(dplyr::c_across(cols = everything()))]))
  
  finalRCP$final_RCP <- gsub(pattern = "_", replacement = "", x = finalRCP$final_RCP)
  
  if(season_vec == "autumn"){
    finalRCP$final_RCP <- factor(finalRCP$final_RCP, levels = c("RCP1", "RCP2", "RCP3"))
  } else {
    finalRCP$final_RCP <- factor(finalRCP$final_RCP, levels = c("RCP1", "RCP2"))
  }
  
  ## Bind with 'rcp_data' to get 'IDgrid'
  finalRCP <- 
    cbind(rcp_data, finalRCP)
  
  finalRCP <- dplyr::select(finalRCP, 
                            final_RCP, all_of(spp_cols))
  
  ## iNEXT: rarefaction curves ----------------------------------------------- #
  lvls <- levels(finalRCP$final_RCP)
  
  # Store data here
  inext_list <- list()
  
  for (lvl in lvls) {
    name <- as.character(lvl)
    inext_list[[name]] <- data.frame(t(finalRCP[finalRCP$final_RCP == lvl, 
                                                2:ncol(finalRCP)]))
    rm("name", "lvl")
  }
  
  ## iNEXT function [q = 0 means spp richness]
  inext_obj <- iNEXT::iNEXT(inext_list, q = 0, datatype = "incidence_raw")
  
  ### Plot curves: 
  ### To customize line width & point size, we need a bit of a hack
  ### Based on (https://stackoverflow.com/questions/63031619/override-aesthetics-of-custom-plot)
  
  ## Sample-size-based R/E curve ----------------------------------------------#
  
  # This would be the 'simple way':
  # curve_sample_size_based <- 
  #   iNEXT::ggiNEXT(inext_obj, type = 1, se = TRUE) + 
  #   scale_colour_brewer(palette = "Dark2") +
  #   scale_fill_brewer(palette = "Dark2") +
  #   xlab("Number of sampling units (grids)") +
  #   ylab("Species richness") + 
  #   theme_bw() + 
  #   theme(legend.position = "none",
  #         axis.title = element_text(size = 8),
  #         axis.text = element_text(size = 6))
  
  # Hack
  df <- fortify(inext_obj, type = 1)
  
  df.point <- df[which(df$Method == "Observed"), ]
  df.line <- df[which(df$Method != "Observed"), ]
  df.line$Method <- factor(df.line$Method, 
                           c("Rarefaction", "Extrapolation"),
                           c("Rarefaction", "Extrapolation"))
  # To ensure colour compatibility with other plots
  if(season_vec == "autumn"){
    df$Assemblage <- factor(df$Assemblage, levels = c("RCP1", "RCP2", "RCP3"))
  } else {
    df$Assemblage <- factor(df$Assemblage, levels = c("RCP1", "RCP2"))
  }
  
  curve_sample_size_based <- 
    ggplot(df, aes(x = x, y = y, colour = Assemblage, fill = Assemblage)) + 
    geom_point(aes(shape = Assemblage), size = 3, data = df.point) +
    geom_line(aes(linetype = Method), lwd = 1.15, data = df.line) +
    geom_ribbon(aes(ymin = y.lwr, ymax = y.upr,
                    fill = Assemblage, colour = NULL), alpha = 0.2) +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    xlab("Number of sampling units (grids)") + ylab("Species richness") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 6))
  
  ## Sample completeness curve ------ (not used) ------------------------------#
  # This would be the 'simple way':
  
  # curve_sample_completeness <-
  #   iNEXT::ggiNEXT(inext_obj, type = 2, se = TRUE) +
  #   scale_colour_brewer(palette = "Dark2") +
  #   scale_fill_brewer(palette = "Dark2") +
  #   theme_bw() +
  #   theme(axis.title = element_text(size = 8),
  #         axis.text = element_text(size = 6))
  
  ## Coverage-based R/E curves ------------------------------------------------#
  
  # This would be the 'simple way':
  # curve_coverage_based <- 
  #   iNEXT::ggiNEXT(inext_obj, type = 3, se = TRUE) +  
  #   scale_colour_brewer(palette = "Dark2") +
  #   scale_fill_brewer(palette = "Dark2") +
  #   ylab("") +
  #   theme_bw() + 
  #   theme(legend.position = "right",
  #         legend.title = element_blank(),
  #         legend.key.size = unit(0.1, "cm"),
  #         legend.text = element_text(size = 6),
  #         axis.title = element_text(size = 8),
  #         axis.text = element_text(size = 6))
  
  # Hack (Note: I'm overwriting previous objects)
  df <- fortify(inext_obj, type = 3)
  
  df.point <- df[which(df$Method == "Observed"), ]
  df.line <- df[which(df$Method != "Observed"), ]
  df.line$Method <- factor(df.line$Method, 
                           c("Rarefaction", "Extrapolation"),
                           c("Rarefaction", "Extrapolation"))
  # To ensure colour compatibility with other plots
  if(season_vec == "autumn"){
    df$Assemblage <- factor(df$Assemblage, levels = c("RCP1", "RCP2", "RCP3"))
  } else {
    df$Assemblage <- factor(df$Assemblage, levels = c("RCP1", "RCP2"))
  }
  
  curve_coverage_based <- 
    ggplot(df, aes(x = x, y = y, colour = Assemblage, fill = Assemblage)) + 
    geom_point(aes(shape = Assemblage), size = 3, data = df.point) +
    geom_line(aes(linetype = Method), lwd = 1.15, data = df.line,
              show.legend = FALSE) +
    geom_ribbon(aes(ymin = y.lwr, ymax = y.upr,
                    fill = Assemblage, colour = NULL), alpha = 0.2) +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    xlab("Sample coverage") + ylab("") +
    theme_bw() + 
    theme(legend.position = c(0.2, 0.8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white", size = 0.1, 
                                           linetype = "solid", colour = "black"),
          legend.key.size = unit(0.45, "cm"),
          legend.text = element_text(size = 6),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 6))
  
  # Patchwork plots & save them
  inext_plot <- 
    curve_sample_size_based + curve_coverage_based 
  
  ggsave(inext_plot, 
         filename = paste0("./results/Bernoulli/Bernoulli_", as.character(season_vec), "_07_iNEXT.png"), 
         height = 7, width = 14, units = "cm", dpi = 300)
  
  ## Clean environment
  rm("rcp_data", "spp_cols", "best_model_files", "pred", "best_model",
     "rcp_select", "finalRCP", "lvls",
     "inext_list", "inext_obj", "df", "df.point", "df.line",
     "inext_plot", "curve_sample_size_based", "curve_coverage_based",
     "season_vec")
  gc()
}
