# pregtb
Estimates of TB burden during pregnancy & postpartum

# Notes on analysis

## Structure

The directory structure is

```         
.
├── TBburden
    ├── indata
    ├── outdata
    ├── plots
    └── R
├── TBrisk
    ├── indata
    ├── outdata
    ├── plots
    └── R
├── Old files

```

All necessary input data to run the analysis is in indata.

## Dependencies

This analysis was run using R version 4.4.1 (2024-06-14).

The following R packages must be available:

-   utility: here, glue, stringr
-   data manipulation: data.table, dplyr,
-   plotting: ggplot2, ggthemes, scales, ggrepel, ggpubr

## Description of folders & analysis scripts

### TBrisk

This folder contains the analysis scripts for estimating TB risk during pregnancy & postpartum.

#### MetaDataPrep.R

This file pre-processes the data required to do the meta-analysis.

#### PregTBRiskMetaanalysis.R

This file performs the meta-analysis to estimate TB risk during pregnancy & postpartum. The main outputs are:
- Forest plot of IRRs for TB during pregnancy & postpartum
- CSV file with the meta-analysis summary estimates for use in the TBburden analysis.


### TBburden

This folder contains the main analysis scripts for estimating TB burden during pregnancy & postpartum.

#### TBrisk_hiv.R

This file processes and saves out the main TB and HIV input data. This includes TB burden, TB risk given HIV, and pregnancy HIV data. The outputs are saved in outdata/TBHIV.Rdata.

#### population_fertility.R

This file processes and saves out the UN population of women in the reproductive age group and fertility/births data. The outputs are saved in outdata/UNPOP.RData.

#### plots.R

This file contains the code to generate the plots for the TB burden analysis. The plots are saved in plots/.

#### maps.R

This file contains the code to generate the maps for the TB burden analysis. The maps are saved in plots/.

#### pregTB_outputs.R

This is the main analysis script that pulls together the pre-processedinput data to estimate TB burden during pregnancy & postpartum. The outputs are saved individually in outdata and include:
- Population of women in the reproductive age group
- Person-time at risk of TB during pregnancy & postpartum
- TB burden during pregnancy & postpartum

#### pregTB_births2023.R

This is an old main analysis script that estimates TB burden during pregnancy & postpartum. `To be deleted`.

### TBrisk

This folder contains old analysis scripts for estimating TB risk during pregnancy & postpartum.`To be deleted/untracked`.
