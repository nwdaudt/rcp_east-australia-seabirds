# Seabird assemblages are linked to the major western boundary current off eastern Australia

This `.scripts/README` guide you through the scripts used in [Daudt *et al.*](https://github.com/nwdaudt/rcp_east-australia-seabirds/tree/main?tab=readme-ov-file#citation) (2024).

There will be two sessions. Jump straight in [modelling](https://github.com/nwdaudt/rcp_east-australia-seabirds/tree/main/scripts#modelling) if you are interested in the results of the paper. The [full pipeline](https://github.com/nwdaudt/rcp_east-australia-seabirds/tree/main/scripts#full-pipeline) explains the logic behind each script and what it does, before getting the the modelling steps.

***
## General workflow and formatation

The scripts are named following a numerical order, so its is easy to follow (numbers relate to script names):

![Fig 1. Scripts workflow](https://github.com/nwdaudt/rcp_east-australia-seabirds/blob/main/scripts/scripts-workflow.png)

All scripts were developed using [RStudio IDE](https://posit.co/products/open-source/rstudio/) and, for the sake of facility, I structured them into 'code sections'. So, if you use RStudio, you can follow the *document outline* easily. 

Below, I provide a 'pseudo-code' underlying what each script does.

***
## Full pipeline

NOTE: Seabird raw data can not be publicly available at the moment, so scripts '01'--'07' are here for the transparency of the process. Environmental data are all publicly available through the links provided within the manuscript (see Table 1 in the main text). The gridded, tidied-up data after script '06' and '07' are available under `./data_out/`, which allows you to reproduce the main results.

* **'00': R environment**
  - Sets up R environment by calling `renv::restore()`
* **'01': Merge and tidy up raw dataset**
  - Merge two raw datasets into one
  - Clean columns, standardise some data and create season column
  - Standardise taxonomy (e.g. common names to scientific names, subspecies to species), and clean taxa that won't be used (e.g. egrets, shorebirds)
  - Brief summary of data (e.g. no. voyages, no. records, no. taxa)
* **'02': Create the grids on which data will be aggregated**
  - Read and subset only East Australia data
  - Transform it into spatial objects
  - Create the grids (0.5--2° latitude)
* **'03': Extracts environmental data**
  - Bathymetry (bat), slope (slope), distance from the coast (dist_coast)
  - Calculate and extract Eddy Kinetic Energy (eke)
  - Extract Chlorophyll-a (chl8) and its log10 (log10_chl8)
  - Crop original Sea Surface Temperature (sst) files and extract values
  - Extract Sea Surface Salinity (sss) and Mixed Layer Depth (mld)
  - Create and extract SST gradient (sst_grad)
  - Create and extract climatic data for SST gradient (clim_sst_grad), Eddy Kinetic Energy mean (clim_eke_mean), Eddy Kinetic Energy standard deviation (clim_eke_sd)
* **'04': Aggregate seabird and environmental data by grids, and create 'final' wide-format dataset (to be used in analyses)**
  - Aggregate data by grids
  - Based on the code section "EDA - how many species by grid/season and grid size", we decided on 1° latitude grids would be best
  - Wrangling and tidy up the dataset for future analyses
* **'05': Exploratory data analyses**
  - General summary (e.g. no. voyages, no. records, no. taxa, no. individuals recorded)
  - No. of individuals by [species/]grid/season
  - No. of occurrences, Frequency of occurrence, Numeric frequency of species, by season
  - Based on the above, get a vector with species names to remove from models
  - Maps with seasonal sampling, no. of species (species richness), no. of birds
* **'06': Prepares data to fit RCP models**
  - Split data by season, scale environmental data (by season) and check correlation between them
* **'07': Creates average climatic seasonal, static layers for all environmental (data for prediction)**
  - Create SST layers
  - Create SSS and MLD layers
  - Create CHL layers
  - Crop and extract data for each season
  - Note: 'clim_eke_sd', 'clim_eke_mean' (consequently, 'eke'), and 'clim_sst_grad' (consequently, 'sst_grad') were already created under script '03'
* **'08_1': Fit Bernoulli RCP models**
  - See below
* **'08_2': Fit Negative Binomial RCP models**
  - See below
* **'09': Prepare Figures for publication**
  - Use {patchwork} to put together figures for publication

## Modelling

* **'08_1': Fit Bernoulli RCP models**
  - Transform abundance data into binary 0/1 (absence/presence)
  - Choose the number of groups (RCPs) based on `ecomix::regional_mix.multifit()`
  - Fix number of RCPs and run `ecomix::regional_mix()`
  - Choose the best model based on the optimal covariates
  - Model diagnostics
  - Bootstrap the best model to get uncertainty in model parameters
  - *Results:* plot species profiles
  - *Results:* predict and map point-predictions
  - *Results:* predict and map uncertainty
  - *Results:* partial plots relating RCP probability agains environmental data
  - *Results:* diversity rarefaction curves based on {iNEXT}
* **'08_2': Fit Negative Binomial RCP models**
  - Choose the number of groups (RCPs) based on `ecomix::regional_mix.multifit()`
  - Fix number of RCPs and run `ecomix::regional_mix()`
  - Choose the best model based on the optimal covariates
  - Model diagnostics
  - Bootstrap the best model to get uncertainty in model parameters
  - *Results:* plot species profiles
  - *Results:* predict and map point-predictions
  - *Results:* predict and map uncertainty
  - *Results:* partial plots relating RCP probability agains environmental data

## Manuscript

After running the above steps, you will get all the results needed to compile the manuscript. The source `RMarkdown` files are in  `./ms_PROOCE`. Note, however, as explained in the main [README](https://github.com/nwdaudt/rcp_east-australia-seabirds/blob/main/README.md), the higher quality Figure 1 from the manuscript was built outside R (although a lower quality version is available through the codes). 