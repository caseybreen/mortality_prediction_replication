## Mortality Modeling of Partially Observed Cohorts Using Administrative Death Records

This repository contains code and materials to replicate ["Predictive Modeling of U.S. Individual-Level Longevity."](https://osf.io/preprints/socarxiv/znsqg/)

### Replication Package

The this repository includes code to replicate all figures and tables in the paper. There are three steps to running the replication code: 

1. Clone this repository
2. Download the required datasets and update paths (when necessary)
3. Run the scrips in numeric order (or the `00_run_all.Rmd` script, which will run all scripts; please note this will take at least 24 hours.)


#### Data 

Please download the CenSoc-DMF file, the CenSoc-Numident file, and the full-count 1940 Census. The data were originally obtained from: 

- IPUMS-USA [[link](https://usa.ipums.org/usa/)]
- CenSoc [[link](https://censoc.berkeley.edu/)]

#### Code 

After downloading the required data and updating paths, researchers can run the following script to replicate all figures and tables: 

- `00_run_all.Rmd` - this file runs all scripts. 

Alternatively, researchers can run each script individually in any order. 

- `01_clean_prep_data_dmf.Rmd` - Prep CenSoc-DMF data for modeling. Selects variables from the 1940 Census and CenSoc-DMF file, drops missing values, and recodes variables, and restricts to the birth cohort of 1910.   
- `02_clean_prep_data_numident.Rmd` - Prep CenSoc-Numident data for modeling. Selects variables from the 1940 Census and CenSoc-DMF file, drops missing values, and recodes variables, and restricts to the birth cohort of 1910.   
- `03_predictions_dmf.Rmd` - Fits an ensemble machine learning algorithm on the CenSoc-DMF training dataset and makes predictions on the CenSoc-DMF holdout dataset. 
- `04_predictions_numident.Rmd` - Fits an ensemble machine learning algorithm on the CenSoc-Numident training dataset and makes predictions on the CenSoc-Numident holdout dataset. 
- `05_descriptive_disparities.Rmd` - Descriptive group-level differences in mortality in the CenSoc-DMF
- `06_sample_representativeness.Rmd` - Creates a figure comparing the composition of our linked sample to the composition of the 1940 Census. 
- `07_period_prediction.Rmd` - Make predictions across many age classes for death in the next 5 years 
- `08_cohort_specific_analysis.Rmd` - Redo main analysis for each birth cohort from 1900-1920 separately



### Authors

- [Casey F. Breen](caseybreen.com)
- [Nathan Seltzer](https://nathanseltzer.github.io/)
