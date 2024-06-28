library(SuperLearner)
library(gridExtra)
library(ggpubr)
library(class)
library(cowplot)

## FUNCTIONS -------------------------------------------------------------------


# Recode function - converts numeric to factor, sets missing values to NA
recodes <- function(df) {
  df2 <- df %>%
    mutate(
      # SEX = factor(SEX),
      # MARST = factor(ifelse(MARST == 3, 4, MARST)), # set separated to divorced
      # RACE = factor(RACE),
      # METRO = factor(METRO),
      # HISPAN = factor(HISPAN),
      # CLASSWKR = factor(CLASSWKR),
      # LABFORCE = factor(LABFORCE),
      # URBAN = factor(URBAN),
      # FARM = factor(FARM),
      # OWNERSHP = factor(OWNERSHP),
      EDUCD = ifelse(EDUCD == 999, NA, EDUCD),
      INCWAGE = ifelse(INCWAGE == 999998 | INCWAGE == 999999, NA, INCWAGE),
      RENT = ifelse(RENT == 9998 | RENT == 9999, NA, RENT))
  
  return(df2)
}

# missing model creator function - models missing data using lm
missing_model_creator <- function(train, test){
  
  registerDoSEQ()
  unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
  }
  
  m_EDUCD <- train(form = EDUCD ~ SEX + INCWAGE + SEI, data = train, method = "lm", na.action = na.omit)
  m_incwage <- train(form = INCWAGE ~ SEX + EDUCD + SEI, data = train, method = "lm", na.action = na.omit)
  m_rent <- train(form = RENT ~ SEX + EDUCD + INCWAGE + SEI, data = train, method = "lm", na.action = na.omit)

  train$EDUCD = ifelse(is.na(train$EDUCD)==TRUE, round(predict(m_EDUCD, train), 0), train$EDUCD)
  train$INCWAGE = ifelse(is.na(train$INCWAGE)==TRUE, round(predict(m_incwage, train), 0), train$INCWAGE)
  train$RENT = ifelse(is.na(train$RENT)==TRUE, round(predict(m_rent, train), 0), train$RENT)

  test$EDUCD = ifelse(is.na(test$EDUCD)==TRUE, round(predict(m_EDUCD, test), 0), test$EDUCD)
  test$INCWAGE = ifelse(is.na(test$INCWAGE)==TRUE, round(predict(m_incwage, test), 0), test$INCWAGE)
  test$RENT = ifelse(is.na(test$RENT)==TRUE, round(predict(m_rent, test), 0), test$RENT)

  train_test <- list(train,test)
  
  return(train_test)
}

# X is our training sample.
x_train = train_data %>%
  select(EDUCD, SEI, INCWAGE, OCCSCORE, PERNUM, NUMPERHH, SEX, RACE, METRO,
         PRESGL, MARST, CITYPOP, NSIBS, RENT, HISPAN, CLASSWKR, MOMLOC, MIGRATE5,
         LABFORCE, NFAMS, URBAN, FARM, OWNERSHP, PRESGL, NCOUPLES, NMOTHERS,
         NFATHERS, MULTGEND, WKSWORK1, WKSWORK2, HRSWORK1, HRSWORK2, MARRNO,
         AGEMARR, CHBORN) 

# Create a holdout set for evaluating model performance.
# Note: cross-validation is even better than a single holdout sample.
x_holdout = test_data %>%
  select(EDUCD, SEI, INCWAGE, OCCSCORE, PERNUM, NUMPERHH, SEX, RACE, METRO,
         PRESGL, MARST, CITYPOP, NSIBS, RENT, HISPAN, CLASSWKR, MOMLOC, MIGRATE5,
         LABFORCE, NFAMS, URBAN, FARM, OWNERSHP, PRESGL, NCOUPLES, NMOTHERS,
         NFATHERS, MULTGEND, WKSWORK1, WKSWORK2, HRSWORK1, HRSWORK2, MARRNO,
         AGEMARR, CHBORN)  


x_train2 <- recodes(x_train)
x_holdout2 <-recodes(x_holdout)
train_test <- missing_model_creator(x_train2, x_holdout2)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

x_train3 <- data.frame(train_test[1]) %>%
  mutate_if(is.numeric, normalize)
x_holdout3 <- data.frame(train_test[2]) %>%
  mutate_if(is.numeric, normalize)


y_train = train_data %>% select(death_age)
y_train = as.vector(y_train$death_age)


y_holdout = test_data %>% select(death_age)
y_holdout = as.vector(y_holdout$death_age)



## Run this if parallel gets stuck and won't turn off
registerDoSEQ()
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

cl <- makePSOCKcluster(8)
registerDoParallel(cl)


sl = SuperLearner(Y = y_train, X = x_train3, family = gaussian(),
                  SL.library = c("SL.lm", "SL.glmnet", "SL.xgboost", "SL.ranger"))


# Turn off parallel processing
stopCluster(cl)

pred = predict(sl, x_holdout3, onlySL = TRUE)

cor(pred$pred, y_holdout)

cor(pred$library.predict[,1], y_holdout) #lm only
cor(pred$library.predict[,2], y_holdout) #glmnet only
cor(pred$library.predict[,3], y_holdout) #xgboost only
cor(pred$library.predict[,4], y_holdout) #ranger (random forest) only

sl

df <- data.frame(#lm = pred$library.predict[,1],
  #glmnet = pred$library.predict[,2],
  lm = predict(sl$fitLibrary$SL.lm_All$object, x_holdout3),
  xgboost = pred$library.predict[,3],
  ranger = pred$library.predict[,4],
  SuperLearner = pred$pred,
  y_holdout)

lm <- ggplot(data = df, aes(x=y_holdout, y=lm)) + 
  geom_point() +
  geom_jitter() + 
  geom_smooth() +
  stat_cor(r.digits = 3) + 
  labs(title = "LM", 
       x = "Actual Age of Death", 
       y = "Predicted Age of Death") + 
  theme_cowplot()
  

glmnet <- ggplot(data = df, aes(x=y_holdout, y=glmnet)) + 
  geom_point() + 
  geom_jitter() +
  geom_smooth() + 
  stat_cor(r.digits = 3) +
  labs(title = "GLMNET",
       x = "Actual Age of Death",
       y = "Predicted Age of Death") + 
  theme_cowplot()


xgboost <- ggplot(data = df, aes(x=y_holdout, y=xgboost)) +
  geom_point() +
  geom_jitter() +
  geom_smooth() + 
  stat_cor(r.digits = 3) + 
  labs(title = "XGBOOST",
       x = "Actual Age of Death",
       y = "Predicted Age of Death") + 
  theme_cowplot()


ranger <- ggplot(data = df, aes(x=y_holdout, y=ranger)) +
  geom_point() + 
  geom_jitter() +
  geom_smooth() + 
  stat_cor(r.digits = 3) + 
  labs(title = "Random Forest (RANGER)", 
       x = "Actual Age of Death",
       y = "Predicted Age of Death") + 
  theme_cowplot()

pred <- ggplot(data = df, aes(x=y_holdout, y=SuperLearner)) + 
  geom_point() + 
  geom_jitter() +
  geom_smooth() +
  stat_cor(r.digits = 3) + 
  labs(title = "SuperLearner",
       x = "Actual Age of Death",
       y = "Predicted Age of Death") + 
  theme_cowplot()

options(scipen == 999)
grid.arrange(lm, xgboost, ranger)
grid.arrange(lm, glmnet, xgboost, ranger)


varImp(sl$fitLibrary$SL.lm_All$object)
sl$fitLibrary$SL.lm_All$object

sl$fitLibrary$SL.xgboost_All$object

