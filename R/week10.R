# Script Settings and Resources
library(tidyverse)
library(haven)
library(caret)
library(tibble)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav") %>%
  mutate_all(~ifelse(.==0, NA, .)) %>%  # turns the 0s into NAs
  drop_na(MOSTHRS) %>%  # removes anyone with NA in MOSTHRS
  rename(work_hours = MOSTHRS) %>%   # changes MOSTHRS label to work_hours
  select(-c(HRS1, HRS2)) %>%   # removes HRS1 and HRS2 variables
  select(-where(~mean(is.na(.))>.75)) %>%  # retains only variables w/ < 75% NAs
  mutate_all(as.numeric) # I was getting errors that the data wasn't usable.

# Visualization
gss_tbl %>%
  ggplot(aes(x=work_hours)) +
  geom_histogram(binwidth = 3, fill = "lightblue", color = "darkblue") +
  labs(title = "Distribution of Work Hours",
       x = "Work Hours",
       y = "Frequency") +
  theme_bw()

# Analysis
#### I still need to turn the tuneGrid and models into ifelse inside the loop.
## Define tuneGrid for all models
ols_grid <- NULL # tuning parameters: intercept only. No tuneGrid needed.
enet_grid <- expand.grid( # tuning parameters: alpha, lambda.
  alpha = c(0, 1), #0=LASSO, 1=ridge 
  lambda = seq(0.0001, 0.1, length = 20) # complexity penalties
  )
rf_grid <- data.frame(  # tuning parameters: mtry, splitrule, min.node.size.
  mtry = c(2, 3, 4, 5, 10, 20, 50, 100), # tuneLength changes mtry
  splitrule = c("variance"),
  min.node.size = 5
  )
xgb_grid <- expand.grid( # tuning parameters: nrounds, max_depth, eta, 
  ## gamma, subsample, colsample_bytree, min_child_weight.
  nrounds = c(2, 5, 10), # boosting iterations
  max_depth = c(2, 5, 10), # max tree depth
  eta = seq(0, 1, length = 10), # shrinkage
  gamma = 0, # minimum loss reduction
  colsample_bytree = 1, # subsample ratio of columns
  min_child_weight = c(1, 2, 3), # min sum of instance weight
  subsample = 1  # subsample percentage
  )

## Define models outside the loop
models <- list(
  "OLS Regression" = list(method = "lm", tuneGrid = ols_grid),
  "Elastic Net" = list(method = "glmnet", tuneGrid = enet_grid),
  "Random Forest" = list(method = "ranger", tuneGrid = rf_grid),
  "eXtreme Gradient Boosting" = list(method = "xgbTree", tuneGrid = xgb_grid)
  )

## Loop over all four models
results <- list()
for (model_name in names(models)) {
  set.seed(8712) # for reproducibility
  
  # Split the dataset into two tibbles, train and holdout
  rows <- sample(nrow(gss_tbl)) # Shuffle row indices
  shuffled <-  gss_tbl[rows,] # randomly order data
  split <- round(nrow(gss_tbl)*.75) # determine row to split on
  train_tbl <- gss_tbl[1:split,] # create train. Only use train() on this tbl
  test_tbl <- gss_tbl[(split+1):nrow(gss_tbl),] # create test set (for holdout)
  
  # Run the models
  model <- train(
    work_hours ~ .,
    train_tbl,
    method = models[[model_name]]$method,
    preProcess = c("center", "scale", "medianImpute"), # imputes missing values
    na.action = na.pass,
    tuneLength = 10,
    trControl = trainControl(method = "cv", # cross-validation
                             number = 10, # 10 fold cv
                             search = "grid",
                             verboseIter = T), # prints additional details
    tuneGrid = models[[model_name]]$tuneGrid
    )
  
  # Save model results
  results[[model_name]] <- list(
    model = model,
    cv_rsq = max(model$results$Rsquared, na.rm = TRUE)
    )
  
  # Predict on holdout set
  predictions <- predict(model, test_tbl, na.action = na.pass)
  results[[model_name]]$ho_rsq <- cor(predictions, test_tbl$work_hours)^2
}


# Publication
## Initialize table1_tbl as an empty list
table1_tbl <- tibble(
  algo = character(),
  cv_rsq = character(),
  ho_rsq = character()
)

## Create the tbl with a loop
for (model_name in names(results)) {
  # Extract CV R-squared and Holdout R-squared from results
  cv_rsq <- str_remove(formatC(results[[model_name]]$cv_rsq,
                               format = 'f', digits = 2), "^0")
  ho_rsq <- str_remove(formatC(results[[model_name]]$ho_rsq,
                               format = 'f', digits = 2), "^0")
  # Create a row for the current model
  model_row <- tibble(
    algo = model_name,
    cv_rsq = cv_rsq,
    ho_rsq = ho_rsq)
  # Append model_row to table1_tbl
  table1_tbl <- bind_rows(table1_tbl, model_row)
}


# Questions
## 1. How did your results change between models? Why do you think this happened, specifically?
results_enetmodel <- enetmodel$results
enetmodel$bestTune # result: alpha = 1, lambda = 0.7368684


# 2. How did your results change between k-fold CV and holdout CV? Why do you think this happened, specifically?
(results_olsmodel <- olsmodel$results) # results: 362.8577 RMSE
olsmodel$bestTune # result: intercept = TRUE
p_olsmodel <- predict(olsmodel, test_tbl, na.action=na.pass) 
error_olsmodel <- p_olsmodel-test_tbl[["work_hours"]]
RMSE_olsmodel <- sqrt(mean(error_olsmodel^2)) # 718.7421 RMSE

RMSE_enetmodel <- sqrt(mean(error_enetmodel^2)) # 13.17391
RMSE_enetmodel.f <- sqrt(mean(error_enetmodel.f^2)) # 10.28183

RMSE_rfmodel <- sqrt(mean(error_rfmodel^2)) # 13.80708

RMSE_xgbmodel <- sqrt(mean(error_xgbmodel^2)) # 22.37695


## Test set RMSE is higher than the training set RMSE because we overfit the 
## training set, and the test set contains data the model hasn't seen before.

# 3. Among the four models, which would you choose for a real-life prediction problem, and why? Are there tradeoffs? Write up to a paragraph.
