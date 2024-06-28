## Project: Using Machine Learning Estimators to Predict Age of Death
## Authors: Nathan Seltzer (UC Berkeley) and Casey Breen (UC Berkeley)
## Created: Feb 2, 2022
## Updated: Feb 8, 2022

## Description: Can age of death be predicted using Census Data? We test this  
##    question using linked historical administrative data from the CenSoc 
##    project, which combines the full count 1940 Census from IPUMS USA with 
##    Social Security Death Records that span the years 1988-2005. We leverage
##    several ML algorithms and approximately 150 features to generate
##    predictions of age of death for birth cohorts born at the beginning of
##    the 20th century. The findings from this research emphasize the promises
##    and perils of using statistical and ML models to attempt to answer one of
##    the most fundamental questions: can death be predicted?

## Workflow: (a) Import Data
##           (b) Clean Data
##           (c) Feature Engineering
##           (d) ML Model Comparison
##           (e) Bootstrap Best Model

## Packages -------------------------------------------------------------------
library(tidyverse) # for various helper functions, pipes, etc.
library(data.table) # for quick import/export of data
library(caret) # for ML model auto-tuning
library(doParallel) # for parallel processing
library(ipumsr) # for variable labels
library(tidymodels) # for data partition

## FUNCTIONS -------------------------------------------------------------------
`%notin%` <- Negate(`%in%`)


## (a) IMPORT DATA -------------------------------------------------------------

### Import 
numident_linked <- fread("/data/josh/CenSoc/censoc_data/censoc_linked_to_census/v2.1/censoc_numident_v2.1_linked.csv")

numident_linked_1910 <- numident_linked %>%
  filter(link_abe_exact_conservative == 1) %>%
  filter(byear == 1910) # %>%
# filter(race_first_cyear < 1988, race_last_cyear < 1988) # %>%
#sample_frac(.2)


set.seed(2232)
split <- initial_split(numident_linked_1910, prop = 0.75) # set to .6
train_data <- training(split )
test_data <- testing(split )
