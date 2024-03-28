# Script Settings and Resources
library(tidyverse)
library(haven)
library(caret)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Data Import and Cleaning
data <- read_sav("../data/GSS2016.sav")
  ## 'user_na=FALSE' is the default. So, I didn't add it. It ensures any 
  ## missing, don’t know, inapplicable, or otherwise not-clearly-answered 
  ## items are marked as missing values.
gss_tbl <- data %>%
  mutate_all(~ifelse(.==0, NA, .)) %>%  # turns the 0s into NAs
  drop_na(MOSTHRS) %>%  # removes anyone with NA in MOSTHRS
  rename(work_hours = MOSTHRS) %>%   # changes MOSTHRS label to work_hours
  select(-c(HRS1, HRS2)) %>%   # removes HRS1 and HRS2 variables
  select(-where(~mean(is.na(.))>.75)) %>%  # retains only variables w/ < 75% NAs
  mutate_all(as.numeric) # I was getting errors that the data wasn't usable.


# Visualization
## includes a visualization of the univariate distribution of work hours
gss_tbl %>%
  ggplot(aes(x=work_hours)) +
  geom_histogram(binwidth = 3, fill = "lightblue", color = "darkblue") +
  labs(title = "Distribution of Work Hours",
       x = "Work Hours",
       y = "Frequency") +
  theme_bw()


# Analysis
## split the dataset into two tibbles, train and holdout
rows <- sample(nrow(gss_tbl)) # Shuffle row indices
shuffled <-  gss_tbl[rows,] # randomly order data
split <- round(nrow(gss_tbl)*.75) # determine row to split on
train_tbl <- gss_tbl[1:split,] # create train. Only use train() on this _tbl
holdout_tbl <- gss_tbl[(split+1):nrow(gss_tbl),] # create test set (holdout)

## define trControl for all models
myControl <- trainControl(method="cv", # cross-validation
                          number=10, # 10 fold cv
                          verboseIter=T) # prints additional details

## define preProcess for all models
myProcess <- c("center", "scale", "medianImpute")
  ## uses median imputation to impute any remaining missing values

## define tuneGrid for all models
# myGrid <- 
  
## run the OLS regression model, "lm"
olsmodel <- train(
  work_hours ~ .,
  train_tbl, 
  method = "lm",
  preProcess = myProcess,
  na.action = na.pass,
  tuneLength = 10, # function will try 10 different values for each tuning parameter in the model. I'm not sure if this is needed.
  trControl = myControl
  ## tuning parameters: intercept. No tuneGrid needed.
  )
## test the OLS regression model
olsmodel$bestTune # result: intercept = TRUE
(p_olsmodel <- predict(olsmodel, holdout_tbl, na.action=na.pass))
(error_olsmodel <- p_olsmodel-gss_tbl[["work_hours"]])
(RMSE_olsmodel <- sqrt(mean(error_olsmodel^2))) # 720.1221

## run the elastic net model, "glmnet"
enetmodel <- train(
  work_hours ~ .,
  train_tbl,
  method = "glmnet",
  preProcess = myProcess,
  na.action = na.pass,
  tuneLength = 10, # function will try 10 different values for each tuning parameter in the model. I'm not sure if this is needed.
  trControl = myControl,
  ## tuning parameters: alpha, lambda.
  tuneGrid = expand.grid(
    alpha = 0:1, 
    lambda = seq(0.0001, 1, length = 20))
  )
## test the elastic net model
enetmodel$bestTune # result: alpha = 36, lambda = 0.7894947
(p_enetmodel <- predict(enetmodel, holdout_tbl, na.action=na.pass))
(error_enetmodel <- p_enetmodel-gss_tbl[["work_hours"]])
(RMSE_enetmodel <- sqrt(mean(error_enetmodel^2))) # 21.99045

## run the random forest model, "ranger"
rfmodel <- train(
  work_hours ~ .,
  train_tbl,
  method = "ranger",
  preProcess = myProcess,
  na.action = na.pass,
  tuneLength = 10, # function will try 10 different values for each tuning parameter in the model. I'm not sure if this is needed.
  trControl = myControl,
  ## tuning parameters: mtry, splitrule, min.node.size.
  tuneGrid = data.frame(
    mtry = c(2, 3, 7),
    splitrule = c("variance", "extratrees"),
    min.node.size = 5)
  )
## test the random forest model
rfmodel$bestTune # result: mtry = 7, splitrule = variance, min.node.size = 5
(p_rfmodel <- predict(rfmodel, holdout_tbl, na.action=na.pass))
(error_rfmodel <- p_rfmodel-gss_tbl[["work_hours"]])
(RMSE_rfmodel <- sqrt(mean(error_rfmodel^2))) # 18.19889

## run the “eXtreme Gradient Boosting” model, "xgbDART"
xgbmodel <- train(
  work_hours ~ .,
  train_tbl,
  method = "xgbDART", # or "xgbTREE"
  preProcess = myProcess,
  na.action = na.pass,
  tuneLength = 10, # function will try 10 different values for each tuning parameter in the model. I'm not sure if this is needed.
  trControl = myControl,
  ## tuning parameters: nrounds, max_depth, eta, gamma, subsample, 
  ## colsample_bytree, -rate_drop, -skip_drop, min_child_weight.
  tuneGrid = expand.grid(
    nrounds = c(2, 5, 10), # boosting iterations
    max_depth = c(2, 5, 10), # max tree depth
    eta = seq(0, 1, length = 10), # shrinkage
    gamma = 0, # minimum loss reduction
    subsample = 1, # subsample percentage
    colsample_bytree = 1, # subsample ratio of columns
    rate_drop = 0, # fraction of trees dropped
    skip_drop = 0, # prob. of skipping drop-out
    min_child_weight = c(1, 2, 3)) # min sum of instance weight
  )
## test the "eXtreme Gradient Boosting" model
xgbmodel$bestTune # results: nrounds = 10, max_depth = 2, eta = 0.444, m_c_w = 3
(p_xgbmodel <- predict(xgbmodel, holdout_tbl, na.action=na.pass))
(error_xgbmodel <- p_xgbmodel-gss_tbl[["work_hours"]])
(RMSE_xgbmodel <- sqrt(mean(error_xgbmodel^2))) # 22.37695
  

# use resamples() to compare output directly
summary(resamples(list(olsmodel, enetmodel, rfmodel, xgbmodel)))

# use plots to look at difference in AUC across models
dotplot(resamples(list(
  olsmodel, enetmodel, rfmodel, xgbmodel), metric = "ROC"))


# calculate the correlation between these predictions and the actual criterion values from test_tbl



  
# Publication
# table1_tbl <- 
## Round all values to 2 decimal places in this tibble and display them without leading zeros and to the hundredths place
  
# 1. How did your results change between models? Why do you think this happened, specifically?
# 2. How did your results change between k-fold CV and holdout CV? Why do you think this happened, specifically?
# 3. Among the four models, which would you choose for a real-life prediction problem, and why? Are there tradeoffs? Write up to a paragraph