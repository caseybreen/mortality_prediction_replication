## Mortality Modeling of Partially Observed Cohorts Using Administrative Death Records

This repository contains code and materials to replicate ["Predictive Modeling of U.S. Individual-Level Longevity."](https://osf.io/preprints/socarxiv/znsqg/)

### Replication Package

The this repository includes code to replicate all figures and tables in the paper. There are three steps to running the replication code: 

1. Clone this repository
2. Download the required datasets and update paths (when necessary)
3. Run the `00_run_all.Rmd` script, which will run all code (or run all scripts individually in any)


#### Data 

Please download the CenSoc-DMF file, the CenSoc-Numident file, and the full-count 1940 Census. The data were originally obtained from: 

- IPUMS-USA [[link](https://usa.ipums.org/usa/)]
- CenSoc [[link](https://censoc.berkeley.edu/)]

#### Code 

After downloading the required data and updating paths, researchers can run the following script to replicate all figures and tables: 

- `00_run_all.Rmd` - this file runs all scripts. 

Alternatively, researchers can run each script individually in any order. 

- `01a_clean_prep_data_dmf` - this script creates the analysis final analysis file used for prediction. Specifically, this script selects variables from the 1940 Census and CenSoc-DMF file, drops missing values, and normalizes variables, and restricts to the birth cohort of 1910.   
- `01b_clean_prep_data_numident` - this script creates the analysis final analysis file used for prediction. Specifically, this script selects variables from the 1940 Census and CenSoc-DMF file, drops missing values, and normalizes variables, and restricts to the birth cohort of 1910.   
- `02a_predictions_dmf.Rmd` - Fits 8 different machine learning algorithms on the CenSoc-DMF training dataset and makes predictions on the CenSoc-DMF holdout dataset. 
- `02b_predictions_numident.Rmd` - Fits 8 different machine learning algorithms on the CenSoc-Numident training dataset and makes predictions on the CenSoc-Numident holdout dataset. 
- `03_sample_representativeness.Rmd` - creates a figure comparing the composition of our linked sample to the composition of the 1940 Census.  
- `04_aggregate_code_plot.Rmd` - group-level mortality disparities in the CenSoc-DMF

### Authors

- [Casey F. Breen](caseybreen.com)
- [Nathan Seltzer](https://nathanseltzer.github.io/)
