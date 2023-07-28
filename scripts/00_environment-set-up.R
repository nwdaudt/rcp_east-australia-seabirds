##
## ASG environment set-up
## 
## Nicholas W Daudt
## ****************************************************************************#

## This directory started with ./data (and its sub-directories) & ./scripts
## If you haven't cloned this repo from GitHub, you need to set up some directories

# dir.create("./data_out")
# dir.create("./EDA")
# dir.create("./results")
# dir.create("./results/Bernoulli")
# dir.create("./results/NegBin")
# dir.create("./docs")

### Install needed libraries

# Install {pacman}, a wrapper for checking/installing packages
install.packages("pacman")

needed_libraries <- c("plyr", "dplyr", "tidyr", "readr", "tibble",
                      "lubridate", "stringr", "purrr", 
                      "ggplot2", "ggspatial", "patchwork", "RColorBrewer", "colorspace", "rnaturalearth", 
                      "sp", "sf", "mapview", "raster", "terra", "rerddap", "rerddapXtracto", "tmap", 
                      "corrplot", "iNEXT")

pacman::p_install(needed_libraries)

rm("needed_libraries")

# You will also need {hadsstR} for calculating sea surface temperature gradients,
# and {ecomix}, which currently is only available on GitHub (14 March 2023), for running RCPs
pacman::p_install_gh("skiptoniam/ecomix@dev")
pacman::p_install_gh("jebyrnes/hadsstR")

## INCLUDE {renv} stuff here
