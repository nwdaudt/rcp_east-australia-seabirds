# Seabird assemblages are linked to the major western boundary current off eastern Australia

Here you will find all data and code needed to reproduce the results from [XXXXX et al. (*in review*) -- *#citation link*]

[//]: # (https://github.com/nwdaudt/rcp_east-australia-seabirds#citation)

A hard version of this repository **will be** archived at Zenodo.

In this study, we run Region of Common Profile (RCP) models to identify and describe assemblages of seabird species off eastern Australia. Using different numerical resolutions (presence-absence and abundance) we ran seasonal Bernoulli and Negative Binomial RCP models (respectively). The model results consistently show a biogeographic barrier at ~34°S, near the latitude at which the East Australian Current separates from the Australian continental slope, suggesting the persistence of these groups at seasonal and macro spatial scales. Given the rapid climatic changes that the East Australian Current is experiencing and its importance in influencing the distribution of seabirds, the methods applied in our study could be replicated to assess possible changes in assemblages, and how they are affected by changing environmental conditions.

***
## Project structure

Workspace is set as follows:

```shell
rcp_east-australia-seabirds
├── README.md
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
│   └── ./NegBin
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
├── ./ms_preprint
│   ├── ms_preprint.Rmd
│   ├── ms_supp_mat.Rmd
│   └── arxiv.sty
└── references.bib
└── rcp_east-australia-seabirds.Rproj
```

In `./data_out`, you will find the data needed to reproduce the analysis (for details, please check the manuscript).

* `grids_0.5-2.rda` = a list of spatial objects (`sf`) with different squared grid sizes, from 0.5° to 2° latitude x longitude;
* `attributes-scaled-env-data-season.rda` = a list giving the attributes (mean and sd) used to scale environmental data, for each season;
* `rm_spp-season_vec.rda` = a list with species name to remove, given the number of minimum occurrence threshold we have used, for each season;
* `env-cols-to-model-season.rda` = a list of the environmental variables to retain (after checking for collinearity), for each season;
* **`rcp-data-season.rda`** = a list with the final dataset to use in the models, for each season;
* **`pred-data-seasons.rda`** = a list with the final environmental dataset to predict the models, for each season.

In `./EDA`, you will find exploratory data analysis plots (mostly .png files).

In `./results`, you will find in the root directory the main figures presented within the manuscript and supplementary material (.png and .csv files), and in the folders `./Bernoulli` and `./NegBin` the specific results for each model and their outputs.

In `./scripts`, you will find all scripts needed to run the analyses and create the outputs.

In `./ms_preprint`, you will find the .Rmd (or .tex) files needed to render the manuscript. I used `rticles::arxiv_article` template; but note: a few *formatting* tweaks were made by hand, editing the `arxiv.sty` file and the `ms_preprint.Rmd` YAML header.

We used an R-project to wrap the project environment around, `rcp_east-australia-seabirds.Rproj`. 

***
## How to run

You should be able to reproduce all results using scripts in `./scripts` and files from `./data_out`. The `./scripts/README.md` file will guide you through each step.

### Environment

`R` packages and their dependencies were captured using `{renv} v. 1.0.3` in the [*lock.file link*]

[//]: # (LINK https://github.com/nwdaudt/rcp_east-australia-seabirds/blob/main/renv.lock )

However, `{renv}` does *not* capture all the computing environment. The OS and `R` version is detailed below. We did not 'dockerised' our computing environment, so any system dependency you will need to deal with yourself (sorry!).

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

[//]: # (Nicholas W. Daudt https://github.com/nwdaudt )

Any bugs, suggestions, or enquires, please feel free to contact XXX or open an issue.

***
## Citation
Please refer to the original paper if using any piece of this repository (code and/or data). This repository is under CC BY 4.0 license.

[//]: # (Daudt, N.W.; Woehler, E.J.; Schofield, M.R.; Smith, R.O.; Bugoni, L.; Rayment, W.J.) 
Seabird assemblages are linked to the major western boundary current off eastern Australia. (*in review*)

Thanks!
