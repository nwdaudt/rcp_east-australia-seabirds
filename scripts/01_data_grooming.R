##
## ASG data grooming
## 
## Nicholas W Daudt
## ****************************************************************************#

## This code joins data from two ASG's datasets, do some gross wrangling,
## and tidy up taxonomy (order, family, species names).
## Finally, these processes are saved under "./data_out/ASG_2016_2021_tidy.csv"

## Libraries ####

library(plyr)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

## Data ####

data <- readr::read_csv("./data/ASG_2016_2021_raw.csv")

# Adding the last ASG voyage
fk210206 <- readr::read_csv("./data/fk210206_birds_raw.csv") 

# Check cols
colnames(data) == colnames(fk210206)

## Row-bind them
data <- rbind(data, fk210206)

rm("fk210206")

## Gross wrangling ####

## Set up right column classes, and create some useful ones *******************#
names(data)

data <- 
  data %>%
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date),
                .after = date) %>% 
  dplyr::mutate(season = ifelse(month == 12 | month == 1 | month == 2, "summer",
                         ifelse(month == 3 | month == 4 | month == 5,  "autumn",
                         ifelse(month == 6 | month == 7 | month == 8,  "winter",
                                                                       "spring"))),
                .after = month)

# as Factor
factor_cols <- c("observer", "voyage", "ship_activity", "sea_state", "windforce", 
                 "cloud_cover", "cloud_cover_okta", "precipitation", "visibility", 
                 "sun_glare", "speciesid", "wov_code",
                 "season")

data[factor_cols] <- lapply(data[factor_cols], as.factor)

# as Numeric
numeric_cols <- c("latitude", "longitude", "ship_course", "ship_speed", "depth", 
                  "salinity", "sea_temperature", "wind_direction", "air_pressure", 
                  "air_temperature", 
                  "total_ct", "feeding_ct", "sitting_on_water_ct", "sitting_on_ice_ct", 
                  "sitting_on_ship_ct", "in_hand_ct", "flying_past_ct", 
                  "accompanying_ct", "following_wake_ct")

data[numeric_cols] <- lapply(data[numeric_cols], as.numeric)

rm("factor_cols", "numeric_cols")

# Clean some empty columns, columns with no interest for analysis, ************#
# and missing values in species ID and geographic coordinates

data <- 
  data %>% 
  # Remove few observations with wrong lat/lon
  dplyr::filter(latitude != 0 & longitude != 0) %>% 
  # Remove observations where species information is missing
  dplyr::filter(!grepl("null", species, ignore.case = TRUE) & 
                !is.na(wov_code) & !is.na(speciesid)) %>% 
  # No need for these columns in the analysis
  dplyr::select(- c(observer, ship_heading, 
                    sitting_on_ice_ct, in_hand_ct, bird_direction))

## Good data - according to Eric **********************************************# **

# levels(data$voyage)

data <- 
  data %>% 
  dplyr::filter(
    # 2016
    voyage == "in2016_t02" | voyage == "in2016_v06" | 
    # 2017
    voyage == "in2017_v02" | voyage == "in2017_t01" | voyage == "in2017_t02" | 
    # 2018
    voyage == "in2018_c01" | voyage == "in2018_t02" | 
    voyage == "in2018_v04" | voyage == "in2018_v06" | 
    # 2019
    voyage == "in2019_t01" | voyage == "in2019_t02" | 
    voyage == "in2019_t03" | voyage == "in2019_v04" | 
    voyage == "in2019_v07" | 
    # 2020
    voyage == "fk201228" | voyage == "fk210206") %>% 
  droplevels(.)

# Replace missing values in 'total_ct' column, with the sum of ****************#
# other counting columns

# Check for NAs
plyr::count(is.na(data$total_ct)) # TRUE  1298

cols_to_sum <- c("feeding_ct", "sitting_on_water_ct", "flying_past_ct", 
                 "accompanying_ct", "following_wake_ct")
## Note: I did not considered records from birds on the ship (col = "sitting_on_ship_ct")
## Ship-followers will be removed after getting environmental data, in script "04".

data <- 
  data %>% 
  dplyr::mutate(total_ct = rowSums(across(all_of(cols_to_sum)), na.rm = TRUE))

# Check for NAs again
plyr::count(is.na(data$total_ct)) # all good

rm("cols_to_sum")

# Exclude rows summing to "0" -- they mean all columns were "NA" or "0" *******#
plyr::count(data$total_ct == 0) # TRUE   655

data <- dplyr::filter(data, total_ct != 0) %>% droplevels(.)

# Check
plyr::count(data$total_ct == 0) # all good

## Taxonomy & seabird species ####

# levels(as.factor(data$species))

data <- 
  data %>%
  # Recode 'common names' to family/genus
  dplyr::mutate(species = dplyr::recode(species, # old = new
                                        "albatross sp." = "Diomedeidae",
                                        "Diomedea royal albatross sp." = "Diomedea sp.",
                                        "Fregatidae sp." = "Fregatidae",
                                        "great albatross" = "Diomedea sp.",
                                        "Hydrobatidae sp." = "Hydrobatidae",
                                        "jaeger sp." = "Stercorarius sp.",
                                        "petrel sp." = "Procellariidae",
                                        "Puffinus spp. indet" = "Puffinus spp.",
                                        "Skua sp." = "Catharacta sp.",
                                        "Spheniscidae sp." = "Spheniscidae",
                                        "tern sp." = "Sternidae")) %>%
  # Recode subspecies to species
  dplyr::mutate(species = dplyr::recode(species, # old = new
                                        "Catharacta lonnbergi lonnbergi" = "Catharacta lonnbergi", 
                                        "Daption capense australe" = "Daption capense",
                                        "Oceanites oceanicus oceanicus" = "Oceanites oceanicus",
                                        "Pterodroma macroptera (macroptera)" = "Pterodroma macroptera",
                                        "Puffinus assimilis assimilis/tunneyi" = "Puffinus assimilis",
                                        "Puffinus assimilis elegans" = "Puffinus elegans")) %>%
  # Recode some genus/species in accordance to current taxonomic nomenclature
  # Following BirdLife International (2021)
  dplyr::mutate(species = dplyr::recode(species, # old = new
                                        "Catharacta lonnbergi" = "Catharacta antartica",
                                        "Phalacrocorax melanoleucos" = "Microcarbo melanoleucos",
                                        "Puffinus carneipes" = "Ardenna carneipes",
                                        "Puffinus griseus" = "Ardenna grisea",
                                        "Puffinus pacificus" = "Ardenna pacifica",
                                        "Puffinus tenuirostris" = "Ardenna tenuirostris",
                                        "Sterna albifrons" = "Sternula albifrons",
                                        "Sterna anaethetus" = "Onychoprion anaethetus",
                                        "Sterna bergii" = "Thalasseus bergii",
                                        "Sterna fuscata" = "Onychoprion fuscatus",
                                        "Sterna nereis" = "Sternula nereis",
                                        "Thalassarche melanophrys" = "Thalassarche melanophris"))

# levels(as.factor(data$species)) # Check it again -- OK

# Check this one - "Identified but not listed": 1 record, removed it in the next step
# data %>% dplyr::filter(species == "Identified but not listed")

## Remove non-seabird taxa
# Although 'Phalacrocorax spp.' can be argued as non "sea"-bird, we kept them

data <- 
  data %>% 
  dplyr::filter(! species %in% 
                  c("Scombridae sp.", # << This is fish
                    "Ardea alba", "Ardea ibis", "Butorides striata", 
                    "Cacatua galerita", "Cacatua roseicapilla", "Ducula spilorrhoa", 
                    "Egretta sacra", "Falco cenchroides", "Falco sp.", 
                    "Haliaeetus leucogaster", "Himantopus himantopus", "Pluvialis fulva", 
                    "Unidentified Bird", "Identified but not listed"))

## Check it -- OK
occ_per_spp <-
  data %>%
  dplyr::count(species) %>%
  dplyr::arrange(n)

rm("occ_per_spp")

## Create new columns to possibly aggregate afterwards
data <- 
  data %>%
  # 'Lowest taxonomic level possible' column
  dplyr::mutate(lowest_taxonomic_level = species, .after = species) %>%
  # 'species' to contain only Species
  dplyr::mutate(species = ifelse(stringr::str_detect(species, "dae$") == TRUE |
                                 stringr::str_detect(species, "sp.") == TRUE |
                                 stringr::str_detect(species, "spp.") == TRUE,
                                 yes = NA, no = species)) %>%
  # Get Genus (note there is still family names in here; these will be removed below)
  dplyr::mutate(genus = 
                  stringr::str_extract(lowest_taxonomic_level, "[A-Za-z]+"), .before = species)

## Family vectors, based on 'genus' column ---- 13 families, 5 orders
# levels(as.factor(data$genus))

# Sphenisciformes
spheniscidae <- c("Eudyptes", "Eudyptula", "Spheniscidae")
# Procellariiformes
diomedeidae <- c("Diomedea", "Diomedeidae", "Phoebetria", "Thalassarche")
procellariidae <- c("Ardenna", "Calonectris", "Daption", "Fulmarus", "Halobaena",
                    "Macronectes", "Pachyptila", "Pelecanoides", "Procellaria",
                    "Procellariidae", "Pterodroma", "Puffinus")
oceanitidae <- c("Fregetta", "Garrodia", "Oceanites", "Pelagodroma")
hydrobatidae <- c("Hydrobatidae")
# Phaethontiformes
phaethontidae <- c("Phaethon")
# Pelecaniformes
pelicanidae <- c("Pelecanus")
# Suliformes
fregatidae <- c("Fregata", "Fregatidae")
sulidae <- c("Morus", "Sula")
phalacrocoracidae <- c("Microcarbo", "Phalacrocorax")
# Charadriformes
sternidae <- c("Anous", "Gygis", "Onychoprion", "Sterna", "Sternidae", 
               "Sternula", "Thalasseus")
laridae <- c("Larus")
stercorariidae <- c("Catharacta", "Stercorarius")

data <- 
  data %>%
  # Set a Family column based on the above-defined vectors
  dplyr::mutate(family = dplyr::case_when(
    genus %in% c(spheniscidae) ~ "Spheniscidae",
    genus %in% c(diomedeidae) ~ "Diomedeidae",
    genus %in% c(procellariidae) ~ "Procellariidae",
    genus %in% c(oceanitidae) ~ "Oceanitidae",
    genus %in% c(hydrobatidae) ~ "Hydrobatidae",
    genus %in% c(phaethontidae) ~ "Phaethontidae",
    genus %in% c(fregatidae) ~ "Fregatidae",
    genus %in% c(sulidae) ~ "Sulidae",
    genus %in% c(phalacrocoracidae) ~ "Phalacrocoracidae",
    genus %in% c(pelicanidae) ~ "Pelicanidae",
    genus %in% c(sternidae) ~ "Sternidae",
    genus %in% c(laridae) ~ "Laridae",
    genus %in% c(stercorariidae) ~ "Stercorariidae"), 
  .before = genus) %>%
  # Set a Order column as well
  dplyr::mutate(order = dplyr::case_when(
    genus %in% c(spheniscidae) ~ "Sphenisciformes",
    genus %in% c(diomedeidae, procellariidae, oceanitidae, hydrobatidae) ~ "Procellariiformes",
    genus %in% c(phaethontidae) ~ "Phaethontiformes",
    genus %in% c(fregatidae, sulidae, phalacrocoracidae) ~ "Suliformes",
    genus %in% c(pelicanidae) ~ "Pelicaniformes",
    genus %in% c(sternidae, laridae, stercorariidae) ~ "Charadriiformes"), 
    .before = family) %>%
  # Set as 'NA' genus that were assigned as 'families' (ends with 'dae')
  dplyr::mutate(genus = 
                  ifelse(stringr::str_detect(genus, "dae$") == TRUE, yes = NA, no = genus))

rm("spheniscidae", "diomedeidae", "procellariidae", "oceanitidae", "hydrobatidae",
   "phaethontidae", "fregatidae", "sulidae", "phalacrocoracidae", "pelicanidae",
   "sternidae", "laridae", "stercorariidae")

## Save it ####

# readr::write_csv(data, "./data_out/ASG_2016_2021_tidy.csv")

## Brief summary numbers ####

length(unique(data$voyage))   # > 16 voyages

min(data$date); max(data$date)
                              # > 2016-08-25; 2021-03-05

nrow(data); plyr::count(data$season)
                              # > 13647 records, in all seasons
                              # > autumn 2827
                              # > spring 4823
                              # > summer 3892
                              # > winter 2105

length(unique(data$species))  # > 87 species
length(unique(data$genus))    # > 37 genus
length(unique(data$family))   # > 13 families
length(unique(data$order))    # > 6 orders

sum(data$total_ct)            # > In total, 158,120 individuals recorded


#### `sessionInfo()` ####

# sessionInfo()

# R version 4.2.0 (2022-04-22)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 20.04.5 LTS
# 
# Matrix products: default
# BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
# LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
# 
# locale:
# [1] LC_CTYPE=en_NZ.UTF-8       LC_NUMERIC=C               LC_TIME=en_NZ.UTF-8       
# [4] LC_COLLATE=en_NZ.UTF-8     LC_MONETARY=en_NZ.UTF-8    LC_MESSAGES=en_NZ.UTF-8   
# [7] LC_PAPER=en_NZ.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
# [10] LC_TELEPHONE=C             LC_MEASUREMENT=en_NZ.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] lubridate_1.8.0 forcats_0.5.1   stringr_1.4.0   dplyr_1.0.9     purrr_0.3.4    
# [6] readr_2.1.2     tidyr_1.2.0     tibble_3.1.7    ggplot2_3.3.6   tidyverse_1.3.1
# [11] plyr_1.8.7     
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.9       cellranger_1.1.0 pillar_1.7.0     compiler_4.2.0   dbplyr_2.2.1    
# [6] tools_4.2.0      jsonlite_1.8.0   lifecycle_1.0.1  gtable_0.3.0     pkgconfig_2.0.3 
# [11] rlang_1.0.4      reprex_2.0.1     DBI_1.1.3        cli_3.3.0        rstudioapi_0.13 
# [16] haven_2.5.0      xml2_1.3.3       withr_2.5.0      httr_1.4.3       fs_1.5.2        
# [21] generics_0.1.3   vctrs_0.4.1      hms_1.1.1        grid_4.2.0       tidyselect_1.1.2
# [26] glue_1.6.2       R6_2.5.1         fansi_1.0.3      readxl_1.4.0     modelr_0.1.8    
# [31] tzdb_0.3.0       magrittr_2.0.3   scales_1.2.0     backports_1.4.1  ellipsis_0.3.2  
# [36] rvest_1.0.2      assertthat_0.2.1 colorspace_2.0-3 utf8_1.2.2       stringi_1.7.6   
# [41] munsell_0.5.0    broom_1.0.0      crayon_1.5.1
