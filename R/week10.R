# Script Settings and Resources
library(tidyverse)
library(haven)
library(caret)
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
#... The Analysis section will contain one executable line in total: a loop iterating through the items contained within that vector or tibble

## split the dataset into two tibbles, train and holdout
rows <- sample(nrow(gss_tbl)) # Shuffle row indices
shuffled <-  gss_tbl[rows,] # randomly order data
split <- round(nrow(gss_tbl)*.75) # determine row to split on
train_tbl <- gss_tbl[1:split,] # create train. Only use train() on this tbl
test_tbl <- gss_tbl[(split+1):nrow(gss_tbl),] # create test set (for holdout)

## define trControl for all models
myControl <- trainControl(method = "cv", # cross-validation
                          number = 10, # 10 fold cv
                          search = "grid",
                          verboseIter = T) # prints additional details
                          
## define preProcess for all models
myProcess <- c("center", "scale", "medianImpute") # imputes missing values

## define tuneGrid for all models
# myGrid <- combine below into if()else()
ols_grid <- NULL # tuning parameters: intercept only. No tuneGrid needed.
enet_grid <- expand.grid( # tuning parameters: alpha, lambda.
  alpha = c(0, 1), #0=LASSO, 1=ridge 
  lambda = seq(0.0001, 0.1, length = 20)) # complexity penalties
rf_grid <- data.frame(  # tuning parameters: mtry, splitrule, min.node.size.
  mtry = c(2, 3, 4, 5, 10, 20, 50, 100), # tuneLength changes mtry
  splitrule = c("variance"),
  min.node.size = 5)
xgb_grid <- expand.grid( # tuning parameters: nrounds, max_depth, eta, 
  ## gamma, subsample, colsample_bytree, min_child_weight.
  nrounds = c(2, 5, 10), # boosting iterations
  max_depth = c(2, 5, 10), # max tree depth
  eta = seq(0, 1, length = 10), # shrinkage
  gamma = 0, # minimum loss reduction
  colsample_bytree = 1, # subsample ratio of columns
  min_child_weight = c(1, 2, 3), # min sum of instance weight
  subsample = 1) # subsample percentage

## Define models
models <- list(
  "OLS Regression" = list(method = "lm", tuneGrid = ols_grid),
  "Elastic Net" = list(method = "glmnet", tuneGrid = enet_grid),
  "Random Forest" = list(method = "ranger", tuneGrid = rf_grid),
  "eXtreme Gradient Boosting" = list(method = "xgbTree", tuneGrid = xgb_grid))

## Loop over models
results <- list()
for (model_name in names(models)) {
  set.seed(8712) # for reproducibility
  model <- train(
    work_hours ~ .,
    train_tbl,
    method = models[[model_name]]$method,
    preProcess = myProcess,
    na.action = na.pass,
    tuneLength = 10,
    trControl = myControl,
    tuneGrid = models[[model_name]]$tuneGrid)
  # Save model results
  results[[model_name]] <- list(
    cv_rsq = max(model$results$Rsquared, na.rm = TRUE),
    model = model)
  # Predict on holdout set
  predictions <- predict(model, test_tbl, na.action = na.pass)
  error <- predictions - test_tbl$work_hours
  results[[model_name]]$ho_rsq <- cor(predictions, test_tbl$work_hours)^2
}

## Print results
for (model_name in names(results)) {
  cat("Model:", model_name, "\n")
  cat("CV R-squared:", results[[model_name]]$cv_rsq, "\n")
  cat("Holdout R-squared:", results[[model_name]]$ho_rsq, "\n\n")
}

#####################################

## run the OLS regression model, "lm"
set.seed(8712) # so I get reproducible results
olsmodel <- train(
  work_hours ~ .,
  train_tbl, 
  method = "lm",
  preProcess = myProcess,
  na.action = na.pass,
  tuneLength = 10, # tries 10 diff values f/each tuning parameter in model.
  trControl = myControl,
  tuneGrid = ols_grid 
  )

### test the OLS regression model
(olsmodel_cv_rsq <- olsmodel$results$Rsquared) # R-sqrd value
### predict on holdout set (test_tbl)
p_olsmodel <- predict(olsmodel, test_tbl, na.action=na.pass) 
error_olsmodel <- p_olsmodel-test_tbl[["work_hours"]]
(olsmodel_ho_rsq <- cor(p_olsmodel, test_tbl$work_hours)^2)


## run the elastic net model, "glmnet"
set.seed(8712) # so I get reproducible results
enetmodel <- train(
  work_hours ~ .,
  train_tbl,
  method = "glmnet",
  preProcess = myProcess,
  na.action = na.pass,
  tuneLength = 10, # tries 10 diff values f/each tuning parameter in model.
  trControl = myControl,
  tuneGrid = enet_grid)
enetmodel # prints model to console

### test the elastic net model
(enetmodel_cv_rsq <- max(enetmodel$results$Rsquared, na.rm = TRUE)) # R-sqrd
### predict on holdout set (test_tbl)
p_enetmodel <- predict(enetmodel, test_tbl, na.action=na.pass)
error_enetmodel <- p_enetmodel-test_tbl[["work_hours"]]
(enetmodel_ho_rsq <- cor(p_enetmodel, test_tbl$work_hours)^2)


## run the random forest model, "ranger"
set.seed(8712) # so I get reproducible results
rfmodel <- train(
  work_hours ~ .,
  train_tbl,
  method = "ranger",
  preProcess = myProcess,
  na.action = na.pass,
  tuneLength = 10, # tries 10 diff values f/each tuning parameter in model.
  trControl = myControl,
  tuneGrid = rf_grid)
rfmodel # prints model to console
plot(rfmodel) # plot model

### test the random forest model
(rfmodel_cv_rsq <- max(rfmodel$results$Rsquared, na.rm = TRUE))
### predict on holdout set (test_tbl)
p_rfmodel <- predict(rfmodel, test_tbl, na.action=na.pass)
error_rfmodel <- p_rfmodel-test_tbl[["work_hours"]]
(rfmodel_ho_rsq <- cor(p_rfmodel, test_tbl$work_hours)^2)

## run the “eXtreme Gradient Boosting” model, "xgbTree"
set.seed(8712) # so I get reproducible results
xgbmodel <- train(
  work_hours ~ .,
  train_tbl,
  method = "xgbTree", # this is the traditional tree-based boosting algorithm
  preProcess = myProcess,
  na.action = na.pass,
  tuneLength = 10, # tries 10 diff values f/each tuning parameter in model.
  trControl = myControl,
  tuneGrid = xgb_grid)
xgbmodel # prints model to console
plot(xgbmodel) # plot model

### test the "eXtreme Gradient Boosting" model
(xgbmodel_cv_rsq <- max(xgbmodel$results$Rsquared, na.rm = TRUE))
### predict on holdout set (test_tbl)
p_xgbmodel <- predict(xgbmodel, test_tbl, na.action=na.pass)
error_xgbmodel <- p_xgbmodel-test_tbl[["work_hours"]]
(xgbmodel_ho_rsq <- cor(p_xgbmodel, test_tbl$work_hours)^2)


# use resamples() to compare output directly
summary(resamples(list(olsmodel, enetmodel, rfmodel, xgbmodel)))

# use plots to look at difference in AUC across models
dotplot(resamples(list( # removed olsmodel because it was so different
  enetmodel, rfmodel, xgbmodel), metric = "ROC"))

  
# Publication
table1_tbl <- tibble(
  algo = c("OLS regression", 
           "elastic net", 
           "random forest", 
           "eXtreme Gradient Boosting"),
  cv_rsq = c( # round all values to 2 decimal places & with no leading zero
    str_remove(formatC(olsmodel_cv_rsq, format = 'f', digits = 2), "^0"),
    str_remove(formatC(enetmodel_cv_rsq, format = 'f', digits = 2), "^0"),
    str_remove(formatC(rfmodel_cv_rsq, format = 'f', digits = 2), "^0"),
    str_remove(formatC(xgbmodel_cv_rsq, format = 'f', digits = 2), "^0")),
  ho_rsq = c( # round all values to 2 decimal places & with no leading zero
    str_remove(formatC(olsmodel_ho_rsq, format = 'f', digits = 2), "^0"),
    str_remove(formatC(enetmodel_ho_rsq, format = 'f', digits = 2), "^0"),
    str_remove(formatC(rfmodel_ho_rsq, format = 'f', digits = 2), "^0"),
    str_remove(formatC(xgbmodel_ho_rsq, format = 'f', digits = 2), "^0")
  )
)
table1_tbl # prints the table

## Questions

# 1. How did your results change between models? Why do you think this happened, specifically?
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
