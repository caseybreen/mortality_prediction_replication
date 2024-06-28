## Source .Rmd files 

source_rmd = function(file, ...) {
  tmp_file = tempfile(fileext=".R")
  on.exit(unlink(tmp_file), add = TRUE)
  knitr::purl(file, output=tmp_file, quiet = T)
  source(file = tmp_file, ...)
}



## recode data 
recodes <- function(df) {
  df2 <- df %>%
    mutate(
      # sex = factor(sex),
      # marst = factor(ifelse(marst == 3, 4, marst)), # set separated to divorced
      # race = factor(race),
      # metro = factor(metro),
      # hispan = factor(hispan),
      # classwkr = factor(classwkr),
      # labforce = factor(labforce),
      # urban = factor(urban),
      # farm = factor(farm),
      # ownershp = factor(ownershp),
      educd = ifelse(educd == 999, NA, educd),
      incwage = ifelse(incwage == 999998 | incwage == 999999, NA, incwage),
      rent = ifelse(rent == 9998 | rent == 9999, NA, rent)
    )
  
  return(df2)
}

## create missing model 
missing_model_creator <- function(df) {
  
  registerDoSEQ()
  unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list = ls(name = env), pos = env)
  }
  
  m_educd <- train(form = educd ~ sex + incwage + sei, data = df, method = "lm", na.action = na.omit)
  m_incwage <- train(form = incwage ~ sex + educd + sei, data = df, method = "lm", na.action = na.omit)
  m_rent <- train(form = rent ~ sex + educd + incwage + sei, data = df, method = "lm", na.action = na.omit)
  
  df$educd <- ifelse(is.na(df$educd) == TRUE, round(predict(m_educd, df), 0), df$educd)
  df$incwage <- ifelse(is.na(df$incwage) == TRUE, round(predict(m_incwage, df), 0), df$incwage)
  df$rent <- ifelse(is.na(df$rent) == TRUE, round(predict(m_rent, df), 0), df$rent)
  
  return(df)
}

## normalize data 
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

## define %notin% command 
`%notin%` <- Negate(`%in%`)



## custom color schemes
cudb <- c("#49b7fc", "#ff7b00", "#17d898", "#ff0083", "#0015ff", "#e5d200", "#999999")
cud <- c("#D55E00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2", "#E69F00", "#F0E442", "#999999")

## library packages 
library(data.table)
library(tidyverse)
library(here)
library(ggrepel)
library(cowplot)
library(socviz)
library(broom)
library(fixest)
library(gt)
library(LexisPlotR)
library(RColorBrewer)
library(ipumsr)
library(gompertztrunc)


## custom functions 

## Source .Rmd 

source_rmd = function(file, ...) {
  tmp_file = tempfile(fileext=".R")
  on.exit(unlink(tmp_file), add = TRUE)
  knitr::purl(file, output=tmp_file, quiet = T)
  source(file = tmp_file, ...)
}


## Recode education 

## function for recoding education
recode_education <- function(df, educ_var) {
  
  df <- df  %>%
    mutate(educ_variable = !!sym(educ_var)) %>%
    mutate(educ_yrs = case_when(
      educ_variable == 2 ~ 0,
      educ_variable == 12 ~ 0,
      educ_variable == 14 ~ 1,
      educ_variable == 15 ~ 2,
      educ_variable == 16 ~ 3,
      educ_variable == 17 ~ 4,
      educ_variable == 22 ~ 5,
      educ_variable == 23 ~ 6,
      educ_variable == 25 ~ 7,
      educ_variable == 26 ~ 8,
      educ_variable == 30 ~ 9,
      educ_variable == 40 ~ 10,
      educ_variable == 50 ~ 11,
      educ_variable == 60 ~ 12,
      educ_variable == 70 ~ 13,
      educ_variable == 80 ~ 14,
      educ_variable == 90 ~ 15,
      educ_variable == 100 ~ 16,
      educ_variable == 110 ~ 17,
      educ_variable == 111 ~ 17,
      educ_variable == 112 ~ 17,
      educ_variable == 113 ~ 17
    )) %>%
    select(-educ_variable)
  
  return(df)
  
}  



calculate_accuracy <- function(data, outcome_var) {
  # Initialize a dataframe to store the results
  results_df <- data.frame(variable = character(), accuracy = numeric(), stringsAsFactors = FALSE)
  
  # Loop through all variables except the outcome variable
  for (var in setdiff(names(data), outcome_var)) {
    # Check if the variable is binary
    if (length(unique(data[[var]])) == 2) {
      # Predict using the variable as a binary predictor
      predictions <- as.factor(data[[var]])
      
      # Get the actual outcome
      actual <- as.factor(data[[outcome_var]])
      
      # Calculate the confusion matrix
      cm <- confusionMatrix(predictions, actual)
      
      # Calculate accuracy
      accuracy <- sum(cm$table[1, 1], cm$table[2, 2]) / sum(cm$table)
      
      # Store the results
      results_df <- rbind(results_df, data.frame(variable = var, accuracy = accuracy))
    }
  }
  
  return(results_df)
}


calculate_mcc <- function(data, outcome_var) {
  
  # Initialize a dataframe to store the results
  results_df <- data.frame(variable = character(), mcc = numeric(), stringsAsFactors = FALSE)
  
  # Loop through all variables except the outcome variable
  for (var in setdiff(names(data), outcome_var)) {
    # Check if the variable is binary
    if (length(unique(data[[var]])) == 2) {
      # Predict using the variable as a binary predictor
      predictions <- data[[var]]
      
      # Get the actual outcome
      actual <- data[[outcome_var]]
      
      # Calculate the confusion matrix
      cm <- confusionMatrix(as.factor(predictions), as.factor(actual))
      
      # Extract the elements of the confusion matrix
      tn <- cm$table[1, 1]
      fp <- cm$table[1, 2]
      fn <- cm$table[2, 1]
      tp <- cm$table[2, 2]
      
      # Calculate MCC
      mcc <- (tp * tn - fp * fn) / (sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) * as.numeric(tn + fp) * as.numeric(tn + fn)))
      
      # Store the results
      results_df <- rbind(results_df, data.frame(variable = var, mcc = mcc))
    }
  }
  
  return(results_df)
}



calculate_r_squared_holdout <- function(data, outcome_var) {
  # Initialize a dataframe to store the results
  results_df <- data.frame(model = character(), r_squared = numeric(), stringsAsFactors = FALSE)
  
  # Get the actual outcome values
  actual <- data[[outcome_var]]
  
  # Loop through all model prediction columns
  model_vars <- setdiff(names(data), outcome_var)
  for (var in model_vars) {
    # Predictions from the model
    predicted <- data[[var]]
    
    # Calculate the mean of the observed outcomes
    mean_training_outcome <- mean(actual)
    
    # Sum of squares of residuals
    ss_res <- sum((actual - predicted)^2)
    
    # Total sum of squares
    ss_tot <- sum((actual - mean_training_outcome)^2)
    
    # Calculate R^2
    r_squared <- 1 - (ss_res / ss_tot)
    
    # Store the results
    results_df <- rbind(results_df, data.frame(model = var, r_squared = r_squared))
  }
  
  return(results_df)
}


calculate_r_squared_for_probabilities <- function(data, outcome_var) {
  # Initialize a dataframe to store the results
  results_df <- data.frame(variable = character(), r_squared = numeric(), stringsAsFactors = FALSE)
  
  # Loop through all variables except the outcome variable
  for (var in setdiff(names(data), outcome_var)) {

      
      # Get the actual outcome values
      actual <- data[[outcome_var]]
      
      # Get the actual outcome values
      predicted <- data[[var]]
      
      # Calculate the mean of the actual outcomes
      mean_actual <- mean(actual, na.rm = TRUE)
      
      # Sum of squares of residuals
      ss_res <- sum((actual - predicted)^2, na.rm = TRUE)
      
      # Total sum of squares
      ss_tot <- sum((actual - mean_actual)^2, na.rm = TRUE)
      
      # Calculate R^2
      r_squared <- 1 - (ss_res / ss_tot)
      
      # Store the results
      results_df <- rbind(results_df, data.frame(variable = var, r_squared = r_squared))
    }
  
  return(results_df)
}

perform_validation <- function(train_data, test_data, dependent_var, features) {
  results_df <- data.frame(feature = character(), r_squared = numeric(), stringsAsFactors = FALSE)
  
  for (feat in features) {
    # Construct the formula
    formula_str <- as.formula(paste(dependent_var, "~", feat))
    
    # Fit the model on training data
    model <- lm(formula = formula_str, data = train_data)
    
    # Predict on testing data
    predictions <- predict(model, newdata = test_data, )
    
    # Calculate R^2
    ss_tot <- sum((test_data[[dependent_var]] - mean(test_data[[dependent_var]]))^2)
    ss_res <- sum((test_data[[dependent_var]] - predictions)^2)
    r_squared <- 1 - (ss_res / ss_tot)
    
    # Store the R^2 value for this feature
    results_df <- rbind(results_df, data.frame(feature = feat, r_squared = r_squared))
  }
  
  # Arrange the results by r_squared in descending order
  results_df <- results_df %>% arrange(desc(r_squared))
  
  return(results_df)
}
