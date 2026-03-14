## Structured Inequality, Stochastic Lifespans: Demographic Perspectives on Predicting Individual-level Longevity

[![OSF](https://img.shields.io/badge/OSF-project-blue)](https://osf.io/5e8wf/)
[![Generic badge](https://img.shields.io/badge/R-4.3.1-orange.svg)](https://cran.r-project.org/bin/macosx/)
[![Generic badge](https://img.shields.io/badge/License-GNU-<green>.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Generic badge](https://img.shields.io/badge/Maintained-Yes-red.svg)]()

This repository contains code and materials to replicate the paper ["Structured Inequality, Stochastic Lifespans: Demographic Perspectives on Predicting Individual-level Longevity."](https://osf.io/preprints/socarxiv/znsqg/)


**Abstract**: There are striking disparities in life expectancy across sociodemographic groups in the United States, shaped by
structural forces such as racism, class inequality, and policy environments. To what extent do sociodemographic characteristics
structure—or fail to structure—individual lifespans? Using U.S. Census data linked to administrative death records, we assess how
well early-adulthood social, economic, and demographic characteristics predict individual lifespan in a cohort of men born in 1910
and observed through their deaths between 1975 and 2005 (N = 121,000). Despite large group-level disparities, we find that sociodemographic
characteristics measured in early adulthood explain less than two percent of the overall variation  in  individual lifespan. 
These findings reaffirm a central demographic regularity: variance in life expectancy between groups is small compared to variation
in lifespan within groups. This highlights the fundamentally non-deterministic nature of how structural inequality shapes individual mortality. 


### Replication Package
------------


The this repository includes code to replicate all figures and tables in the paper. There are three steps to running the replication code: 

1. Clone this repository
2. Download the required datasets and update paths (when necessary)
3. Run the scrips in numeric order (or the `00_run_all.Rmd` script, which will run all scripts. (Please note this will take 24-48 hours.)


### Data 
------------


Please download the CenSoc-DMF file, the CenSoc-Numident file, and the full-count 1940 Census. The data were originally obtained from: 

- IPUMS-USA [[link](https://usa.ipums.org/usa/)]
- CenSoc [[link](https://censoc.berkeley.edu/)]

Alternatively, download the `data.zip` file from the Open Science Framework repository. At the root level of this repository (alongside the `code/` folder), unzip 
the archive to create a `data/` folder containing all required data files for the analysis.


### Code 
------------


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
------------

- [Casey F. Breen](caseybreen.com)
- [Nathan Seltzer](https://nathanseltzer.github.io/)

### Replication
------------

All analyses and computations were carried out on 2023 MacBook Pro with an Apple M2 Pro chip, 16GB memory, and Sonoma 14.1 operating system.

All analyses were originally conducted using R version 4.3.1 and the package versions recorded in the attached session info at the bottom of the README. 
Re-running the pipeline with updated R or package versions, or a different seed, may yield minor numerical differences. 
These do not affect the paper’s results or conclusions.


### Acknowledgements 
------------

For helpful discussions and feedback, we thank Hal Caswell, Jenn Dowd, Aashish Gupta, Dennis Feehan, Joshua R. Goldstein,
Pat Hastings, Ridhi Kashyap, Patricia McManus, Chris Muller, Michelle Niemann, Jenna Nobles, Mathew Salganik, Charlie Rahal,
Alyson Van Raalte, Ken Wachter, Elizabeth Wrigley-Field, participants of the Berkeley CenSoc working group, participants in 
the Oxford health inequalities reading group, participants in the PAA 2023 "Socioeconomic Inequalities in Mortality" session,
and participants in the ASA 2022 ``Computational Sociology: Methods and Applications'' session. C.F.B. was supported by the
National Institute of Aging T32-AG000246. The research reported in this publication was supported by National Institute on Aging
(NIA) (R01AG05894) and infrastructure grants from the Eunice Kennedy Shriver National Institute of Child Health and Human Development 
(P2CHD042849) and the NIA (P30AG066614).


## Session info
------------


```
> sessionInfo()
R version 4.5.1 (2025-06-13)
Platform: aarch64-apple-darwin20
Running under: macOS Tahoe 26.3.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/Chicago
tzcode source: internal

attached base packages:
[1] parallel  splines   stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] ggpubr_0.6.2        earth_5.3.4         plotmo_3.6.4        plotrix_3.8-4      
 [5] Formula_1.2-5       glmnet_4.1-10       Matrix_1.7-3        xgboost_1.7.11.1   
 [9] gbm_2.2.2           ranger_0.17.0       doParallel_1.0.17   iterators_1.0.14   
[13] sl3_1.4.5           SuperLearner_2.0-29 gam_1.22-6          foreach_1.5.2      
[17] nnls_1.6            origami_1.0.7       knitr_1.50          ipumsr_0.9.0       
[21] RColorBrewer_1.1-3  LexisPlotR_0.4.0    rsample_1.3.1       gt_1.1.0           
[25] fixest_0.13.2       broom_1.0.9         cowplot_1.2.0       ggrepel_0.9.7      
[29] lubridate_1.9.4     forcats_1.0.0       stringr_1.5.1       dplyr_1.1.4        
[33] purrr_1.1.0         readr_2.1.5         tidyr_1.3.1         tibble_3.3.0       
[37] ggplot2_3.5.2       tidyverse_2.0.0     data.table_1.17.8   pacman_0.5.1       
[41] here_1.0.1         

loaded via a namespace (and not attached):
  [1] shape_1.4.6.1        rstudioapi_0.17.1    jsonlite_2.0.0       magrittr_2.0.4      
  [5] estimability_1.5.1   farver_2.1.2         rmarkdown_2.29       fs_1.6.6            
  [9] vctrs_0.7.1          ROCR_1.0-11          rstatix_0.7.3        htmltools_0.5.8.1   
 [13] progress_1.2.3       haven_2.5.5          pROC_1.19.0.1        caret_7.0-1         
 [17] rstackdeque_1.1.1    parallelly_1.45.1    htmlwidgets_1.6.4    plyr_1.8.9          
 [21] sandwich_3.1-1       emmeans_2.0.1        zoo_1.8-14           uuid_1.2-1          
 [25] igraph_2.2.2         lifecycle_1.0.5      pkgconfig_2.0.3      R6_2.6.1            
 [29] fastmap_1.2.0        rbibutils_2.3        future_1.67.0        digest_0.6.37       
 [33] numDeriv_2016.8-1.1  furrr_0.3.1          rprojroot_2.1.1      timechange_0.3.0    
 [37] abind_1.4-8          compiler_4.5.1       delayed_0.5.0        withr_3.0.2         
 [41] S7_0.2.0             backports_1.5.0      carData_3.0-5        R.utils_2.13.0      
 [45] ggsignif_0.6.4       MASS_7.3-65          lava_1.8.1           ModelMetrics_1.2.2.2
 [49] tools_4.5.1          future.apply_1.20.0  nnet_7.3-20          R.oo_1.27.1         
 [53] glue_1.8.0           nlme_3.1-168         stringmagic_1.2.0    grid_4.5.1          
 [57] checkmate_2.3.3      reshape2_1.4.4       generics_0.1.4       recipes_1.3.1       
 [61] gtable_0.3.6         tzdb_0.5.0           R.methodsS3_1.8.2    class_7.3-23        
 [65] hms_1.1.3            car_3.1-3            xml2_1.4.0           pillar_1.11.0       
 [69] BBmisc_1.13          lattice_0.22-7       survival_3.8-3       dreamerr_1.5.0      
 [73] tidyselect_1.2.1     stats4_4.5.1         xfun_0.53            hardhat_1.4.2       
 [77] timeDate_4051.111    visNetwork_2.1.4     stringi_1.8.7        yaml_2.3.10         
 [81] evaluate_1.0.5       codetools_0.2-20     cli_3.6.5            rpart_4.1.24        
 [85] xtable_1.8-4         Rdpack_2.6.4         Rcpp_1.1.0           globals_0.18.0      
 [89] zeallot_0.2.0        coda_0.19-4.1        gower_1.0.2          assertthat_0.2.1    
 [93] prettyunits_1.2.0    listenv_0.9.1        mvtnorm_1.3-3        ipred_0.9-15        
 [97] scales_1.4.0         prodlim_2025.04.28   crayon_1.5.3         rlang_1.1.7    
 ```
