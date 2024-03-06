# Seabird assemblages are linked to the major western boundary current off eastern Australia

Here you will find all data and code needed to reproduce and replicate [Daudt *et al.*](https://github.com/nwdaudt/rcp_east-australia-seabirds/tree/main?tab=readme-ov-file#citation) (2024).

A hard version of this repository is archived at the [Open Science Framework](https://osf.io/n582d/).

In this study, we specified Region of Common Profile (RCP) models to identify and describe assemblages of seabird species off eastern Australia. Using different numerical resolutions (presence-absence and abundance) we ran seasonal Bernoulli and negative binomial RCP models, respectively. The model results consistently show a biogeographic barrier near the latitude at which the East Australian Current (EAC) separates from the Australian continental slope, suggesting the persistence of these groups at seasonal and macro spatial scales. Given the rapid climatic changes that the EAC is experiencing and its importance in influencing the distribution of seabirds, the methods applied in our study can be replicated to assess possible changes in assemblages, and how they are affected by changing environmental conditions. The methods can also be applied elsewhere to different taxa and scenarios.

***
## Project structure

Workspace is set as follows:

```shell
rcp_east-australia-seabirds
├── README.md
├── [./data] ** untracked folder containing raw data
├── ./data_out
│   ├── grids_0.5-2.rda
│   ├── attributes-scaled-env-data-season.rda
│   ├── rm_spp-season_vec.rda
│   ├── env-cols-to-model-season.rda
│   ├── rcp-data-season.rda
│   └── pred-data-seasons.rda
├── ./EDA
├── ./results
│   ├── ./Bernoulli
│   ├── ./NegBin
│   └── PNG and CSV files with final results
├── ./scripts
│   ├── README.md
│   ├── 00_environment-set-up.R
│   ├── 01_data_grooming.R
│   ├── 02_data_create-grids.R
│   ├── 03_data_extract-env-data.R
│   ├── 04_data_aggregate-by-grids-and-tidy-it-up.R
│   ├── 05_data_EDA.R
│   ├── 06_data_corr-env-vars_prep-to-fit-RCPs.R
│   ├── 07_data_env-data-for-prediction.R
│   ├── 08_1_modelling_fit-RCPs-Bernoulli.R
│   ├── 08_2_modelling_fit-RCPs-NegBin.R
│   ├── 09_figs-patchwork.R
│   └── source.R
├── ./ms_PROOCE
│   ├── ms_PROOCE.Rmd
│   ├── ms_PROOCE_revised-markup.Rmd
│   ├── ms_PROOCE_supplementary-material.Rmd
│   └── ms_PROOCE_review1_response-to-reviewers.Rmd
└── references.bib
└── rcp_east-australia-seabirds.Rproj
```

In `./data_out`, you will find the data needed to reproduce the analyses (for details, please check the manuscript).

- These files are used to generate the results:
* **`rcp-data-season.rda`** = a list with the final data set to use in the models, for each season;
* **`pred-data-seasons.rda`** = a list with the final environmental data set to predict the models, for each season.

- Auxiliary files used along the process to get to the (above) final data sets:
* `grids_0.5-2.rda` = a list of spatial objects (`sf`) with different squared grid sizes, from 0.5° to 2° latitude x longitude;
* `attributes-scaled-env-data-season.rda` = a list giving the attributes (mean and sd) used to scale environmental data, for each season;
* `rm_spp-season_vec.rda` = a list with species name to remove, given the number of minimum occurrence threshold I have used, for each season;
* `env-cols-to-model-season.rda` = a list of the environmental variables to retain (after checking for collinearity), for each season;

In `./EDA`, you will find exploratory data analysis plots (mostly .png files). *Note*: although Fig 1 in the manuscript has a version built within R (in "./results"), it does not have proper quality for publication. So, I've created it outside R, using Inkscape software---it lives under `.EDA/Fig1-study-area-raw-counts.png`.

In `./results`, you will find in the root directory the main figures presented within the manuscript and supplementary material (.png and .csv files), and in the folders `./Bernoulli` and `./NegBin` the specific results for each model and their outputs.

In `./scripts`, you will find all scripts needed to run the analyses and create the outputs. This folder has its own [README](https://github.com/nwdaudt/rcp_east-australia-seabirds/blob/main/scripts/README.md) to guide you through the files.

In `./ms_PROOCE`, you will find the .Rmd source files needed to render the main manuscript and supplementary materials. I used `rticles::elsevier_article` template.
* `ms_PROOCE.Rmd` = manuscript *submitted* to the journal;
* **`ms_PROOCE_revised-markup.Rmd`** = mark-up version of the revised, *accepted* manuscript;
* **`ms_PROOCE_supplementary-material.Rmd`** = supplementary material;
* `ms_PROOCE_review1_response-to-reviewers.Rmd` = reply to reviewers;

All bibliography cited within the manuscript and supplementary material lives in the root folder, in the `references.bib` file.

I used an R-project to wrap the project environment around, `rcp_east-australia-seabirds.Rproj`. 

***
## How to run

You should be able to reproduce all results using scripts in `./scripts` and files from `./data_out`. The `./scripts/README.md` file will guide you through each step.

### Environment

`R` packages and their dependencies were captured using `{renv} v. 1.0.3` in the [lock.file](https://github.com/nwdaudt/rcp_east-australia-seabirds/blob/main/renv.lock)

However, `{renv}` does *not* capture all the computing environment. The Operational System (OS) and `R` version is detailed below. We did not 'dockerised' our computing environment, so any system dependency you will need to deal with yourself (sorry!).

```shell
R version 4.2.0 (2022-04-22)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 20.04.5 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0

locale:
[1] LC_CTYPE=en_NZ.UTF-8       LC_NUMERIC=C               LC_TIME=en_NZ.UTF-8       
[4] LC_COLLATE=en_NZ.UTF-8     LC_MONETARY=en_NZ.UTF-8    LC_MESSAGES=en_NZ.UTF-8   
[7] LC_PAPER=en_NZ.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C            LC_MEASUREMENT=en_NZ.UTF-8 LC_IDENTIFICATION=C       
```

A heads up -- you will need `R` **4.2.0** and then make sure to `renv::restore()` to load the project environment with the same package versions as ours (if you are not familiar with `{renv}`, see [their website](https://rstudio.github.io/renv/articles/renv.html)). 

***
## Contributors

[Nicholas W. Daudt](https://github.com/nwdaudt)

Any bugs, suggestions, or enquires, please feel free to contact me or open an issue.

***
## Citation
Please refer to the original paper if using any piece of this repository (code and/or data). This repository and the published, open-access article are under CC BY 4.0 license.

Daudt, NW; Woehler, EJ; Schofield, MR; Smith, RO; Bugoni, L; Rayment, WJ. (2024). Seabird assemblages are linked to the major western boundary current off eastern Australia. Progress in Oceanography, https://doi.org/10.1016/j.pocean.2024.103215

[Open Science Framework hard-copy of this repository](https://osf.io/n582d/).
