##
## ASG data -- LDA
## 
## Nicholas W Daudt
## ****************************************************************************#

## Libraries ####

library(dplyr)
library(lda)
# library(sf)

## Data ####

# spring_g1_sf <- sf::st_read("./data_out/grids_spring_g1.gpkg")

spring_g1_df <- read.csv("./data_out/grids_spring_g1.csv")

## Wrangling for analysis ####

### Get IDgrids
id_grids <- unique(spring_g1_df$IDgrid)

### Get a zero-indexed list of seabird species to be used in the topic model
spp_index <- data.frame(
  species_index = 0:(length(unique(spring_g1_df$species)) -1),
  species = unique(spring_g1_df$species)
  )

### Group by & summarise data by species/grid
agg_spring <- 
  spring_g1_df %>% 
  dplyr::group_by(IDgrid, species) %>% 
  dplyr::summarise(n = sum(total_ct)) %>%
  dplyr::ungroup(.)

### Create a column with respective species_index value for each species
agg_spring$species_index <- NA

for (i in 1:length(spp_index$species_index)) {
  agg_spring$species_index[which(agg_spring$species == spp_index$species[i])] <- spp_index$species_index[i]
  rm("i")
  }

### Generate data for {lda} ----------------------------------------------------#
lda_data <- list()

for (id_grid in id_grids) {
  
  # Filter IDgrid
  x <- dplyr::filter(agg_spring, IDgrid == id_grid) %>% dplyr::select(species_index, n)
  # Fill 'species_index' with all indexes
  x <- 
    dplyr::left_join(data.frame(species_index = as.integer(spp_index$species_index)), x,
                     by = "species_index")
  # Replace 'NA' values from *join to zero
  x$n <- as.integer(tidyr::replace_na(x$n, 0))
  # Transpose and clear row/column names
  x <- t(x)
  # rownames(x) <- NULL
  # colnames(x) <- NULL
  
  # Guarantee it is a matrix
  x <- as.matrix(as.data.frame(x))
  
  # Feed it into 'lda_data'
  name <- paste0("grid_", as.character(id_grid))
  lda_data[[name]] <- x
  
  rm("x", "name")
}

rm("id_grid")

## {lda} ####

lda_mod <- 
  lda::lda.collapsed.gibbs.sampler(
    documents = lda_data,
    vocab = spp_index$species,
    K = 3,                          # Check 'K'
    num.iterations = 10000,         # Increase 'num.interactions'? (10,000 took less then a min)
    alpha = 1,                      # Check 'alpha'
    eta = 1,                        # Check 'eta'
    compute.log.likelihood = TRUE
  )

### (return object explained...)
# A list of the assignments (length == document length), 
# indicating the topic assignment ('K') for each word ('species')
lda_mod[["assignments"]]

# How many times a word ('species') was assigned to each topic ('K') group
lda_mod[["topics"]]

# The total number of times words ('species') were assigned to each topic ('K') group
lda_mod[["topic_sums"]]

# Indicates the number of times words ('species') in each document (column) were 
# assigned to each topic (row)
lda_mod[["document_sums"]]

## LDA viz #####

# Some ideas from other works

# 1. Word ('species') count & cloud by topic (group, 'K') -- probably not
# 2. Overall word frequency ('species %FN') and by topic (group, 'K') -- bar plot
# 3. Clustering/Ordination using relative frequencies of words ('species') -- ordination? {gllvm}
# 4. The number of documents for each topic ('K') by assigning the document ('grid') 
#    to the topic that has the most weight in that document -- map



