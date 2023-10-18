##
## ASG - environment set-up
## 
## *************************************************************************** #

## The project started off with the directories "./data" (and its sub-directories) 
## and "./scripts". If you haven't cloned this repo from GitHub, you need to 
## create some directories manually

# dir.create("./data_out")
# dir.create("./EDA")
# dir.create("./results")
# dir.create("./results/Bernoulli")
# dir.create("./results/NegBin")

# The directory "./ms_preprint" was created when I manually opened an Rmarkdown
# file using {rticle}'s template for `arXiv` preprints.

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
##
## NOTE: 
##
## Package versions and their dependencies were captured using {renv} and 
## locked in the `lock.file` file
##
## However, below you'll find the code to install them all, although we can not
## guarantee their versions will be the same if you don't use `renv::restore()`
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Install needed libraries

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
# and {ecomix} for running RCP models, which currently (14 March 2023) is only available on GitHub
pacman::p_install_gh("skiptoniam/ecomix@dev")
pacman::p_install_gh("jebyrnes/hadsstR")

