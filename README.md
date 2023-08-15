# Seabird assemblages are linked to the major western boundary current off eastern Australia

Here you will find all data and code needed to reproduce the results from Daudt et al. (*in prep*). 

A hard-version of this repository *will be* archived at Zenodo.

In this study, **we ... (quickly summarise the paper)**

***
## Project structure

Workspace is set as follows, and we used an .Rproj file to wrap it.

```shell
review_plastic-waterbirds_Brazil
├── README.md
├── ./data_out
│   ├── grids_0.5-2.rda
│   ├── attributes-scaled-env-data-season.rda
│   ├── rm_spp-season_vec.rda
│   ├── rcp-data-season.rda
│   ├── env-cols-to-model-season.rda
│   └── pred-data-seasons.rda
├── ./EDA
│   └── Exploratory Data Analysis plots (mostly .png plots)
├── ./results
│   ├── Main results (.png and .csv files)
│   ├── ../Bernoulli
│   └── ../NegBin
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
├── ./docs
│   ├── ms-draft.Rmd
│   ├── author-info-blocks.lua
│   ├── scholarly-metadata.lua
│   └── docx-template.docx
└── references.bib
└── rcp_east-australia-seabirds.Rproj
```

In `data_out` you will find the data needed to reproduce the analysis.
* `grids_0.5-2.rda` = ;
* `attributes-scaled-env-data-season.rda` = ;
* `rm_spp-season_vec.rda` = ;
* `rcp-data-season.rda` = ;
* `env-cols-to-model-season.rda` = ;
* `pred-data-seasons.rda` = .

In `EDA` you will find exploratory data analysis plots.

In `results` you will find in the root directory the main figures presented within the manuscript and supplementary material, and in the folders `../Bernoulli` and `../NegBin` the specific results for each model and their graphical outputs.

In `scripts` you will find all scripts needed to run the analysese and create the outputs from them.

In `docs` you will find the files needed to render the manuscript content.

## How to run

You should be able to reproduce all results using scrips in `scripts` and files from `data_out`. The `./scripts/README.md` file will guide you on each step.

A heads up -- you will need R **4.2.0** and then make sure to `renv::restore()` to load the project environment with the same package versions.

---
## Contributors

[Nicholas W. Daudt](https://github.com/nwdaudt). **& Skip?**

***
## Citation
Please refer to the original article if using any piece of this repository (code and/or data). This repository is under CC BY 4.0 license.

Daudt, N.W.; Woehler, E.J.; **Woolley, S.N.C.**; Schofield, M.R.; Smith, R.O.; Bugoni, L.; Rayment, W.J. Seabird assemblages are linked to the major western boundary current off eastern Australia. (*in prep*)

Thanks!
