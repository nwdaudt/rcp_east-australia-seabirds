##
## ASG -- Data prep for running the models
## 
## Nicholas W Daudt
## ****************************************************************************#

## This code prepares data for specifying RCPs.

## Libraries ####

library(dplyr)
library(tidyr)
library(corrplot)
# library(PerformanceAnalytics) # at the end, didn't use it

## Data #### 

data <- read.csv("./data_out/seabirds_rcp.csv")
data <- head(data, -1)

spp_cols <- names(data[, 5:84])
env_cols <- names(data[, 85:ncol(data)])

# Vector to remove spp based on number of occurrences
load("./data_out/rm_spp-season_vec.rda")

## RCP data & scaled env data, by season ####

seasons_vec <- c("summer", "autumn", "winter", "spring")

rcp_data_seasons <- list()
env_data_seasons <- list()

for (season_vec in seasons_vec) {
  
  ## Filter season
  df <- dplyr::filter(data, season == season_vec)
  
  ## Get only complete cases
  df <- df[complete.cases(df), ]
  
  ## Scale vars
  # Get environmental data only
  env_vars <- df[, env_cols]
  
  # Scale and store in an object to use attributes later on on prediction grids
  # name the obj, store in the list
  name_env <- paste0("env_scaled_", as.character(season_vec))
  
  env_scaled <- scale(env_vars)
  env_data_seasons[[name_env]] <- env_scaled
  
  ## Merge scaled data 
  rcp_data <- 
    as.data.frame(df) %>% 
    dplyr::select(- all_of(env_cols)) %>%
    cbind(., as.data.frame(env_scaled)) %>%
    dplyr::relocate(all_of(env_cols), .after = lat)
  
  rm_vec <- 
    unlist(remove_spp_vec[grepl(pattern = as.character(season_vec), 
                         x = names(remove_spp_vec))])
  
  name_rcp <- paste0("rcp_data_", as.character(season_vec))
  
  rcp_data_seasons[[name_rcp]] <- rcp_data %>% dplyr::select(- all_of(rm_vec))
  
  rm("df", "env_vars", "name_env", "env_scaled", "rm_vec", "name_rcp",
     "rcp_data", "season_vec")
}

save("rcp_data_seasons", file = "./data_out/rcp-data-season.rda")
save("env_data_seasons", file = "./data_out/attributes-scaled-env-data-season.rda")

## Env vars correlation, by season ####

# load("./data_out/rcp-data-season.rda")
# seasons_vec <- c("summer", "autumn", "winter", "spring")

for (season_vec in seasons_vec) {
  
  ## Read data
  rcp_data <- rcp_data_seasons[grepl(pattern = as.character(season_vec), 
                                     x = names(rcp_data_seasons))]
  rcp_data <- rcp_data[[1]]
  
  env_cols <- names(rcp_data[, 5:17])
  
  # ## Prep to check VIF 
  # (1 - do we need this ???)
  # (2 - is that the way we would do for multivar data ???)
  # mod <- glm(Ardenna.carneipes ~ 
  #              bat_mean + slope_mean + dist_coast_mean + eke_mean + 
  #              chl_mean + chl_log10_mean + sst_mean + sss_mean + mld_mean + 
  #              sst_grad_mean + clim_sst_grad_mean + clim_eke_mean_mean + clim_eke_sd_mean,
  #            data = rcp_data, family = poisson())
  # 
  # # Create vector of VIF values
  # vif_values <- car::vif(mod)
  
  ## Plot correlation between vars (& VIF?)
  file_name <- paste0("./EDA/EDA_env-vars-correlation_", as.character(season_vec), ".png")
  
  png(file = file_name, height = 12, width = 12, units = "cm", res = 300)
  
  # par(mfrow = c(1, 2))
  
  corrplot::corrplot.mixed(
    cor(rcp_data[, colnames(rcp_data) %in% c(env_cols)]),
    lower = "number",
    upper = "circle",
    tl.col = "black", tl.pos = "lt", 
    tl.cex = 0.75, cl.cex = 0.75, number.cex = 0.55,
    lower.col = COL2("BrBG"), upper.col = COL2("BrBG"))
  
  # barplot(vif_values), main = "VIF Values", col = "steelblue")
  # abline(h = 5, lwd = 3, lty = 2)
  
  dev.off()
  
  # # Back to default
  # par(mfrow = c(1, 1))
  
  rm("season_vec", "rcp_data", "file_name")
}

## Visually inspecting the plots, I've decided to remove some variables 
## to avoid collinearity (r > |0.7|)

env_cols_models_season <- list(
  summer = env_cols[! env_cols %in% 
                      c("dist_coast_mean", "chl_mean", "chl_log10_mean", 
                        "clim_sst_grad_mean", "clim_eke_mean_mean", "clim_eke_sd_mean")],
  autumn = env_cols[! env_cols %in% 
                      c("dist_coast_mean", "chl_log10_mean", 
                        "clim_eke_mean_mean", "clim_eke_sd_mean")],
  winter = env_cols[! env_cols %in% 
                      c("dist_coast_mean", "chl_mean", "chl_log10_mean", 
                        "clim_eke_mean_mean", "clim_eke_sd_mean")],
  spring = env_cols[! env_cols %in% 
                      c("chl_log10_mean", "clim_eke_sd_mean")]
)

save("env_cols_models_season", file = "./data_out/env-cols-to-model-season.rda")

# Variables to be used in full models, for each season
for (i in 1:length(env_cols_models_season)) {
  print(names(env_cols_models_season[i]))
  print(paste(env_cols_models_season[[i]], collapse = "+"))
  rm("i")
}
