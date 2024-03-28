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


# Analysis... The Analysis section will contain one executable line in total: a loop iterating through the items contained within that vector or tibble

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
myProcess <- c("center", "scale", "medianImpute")
  ## uses median imputation to impute any remaining missing values

## define tuneGrid for all models, ifelse() to try a loop
# myGrid <- 
  

## run the OLS regression model, "lm"
set.seed(8712) # so I get reproducible results
olsmodel <- train(
  work_hours ~ .,
  train_tbl, 
  method = "lm",
  preProcess = myProcess,
  na.action = na.pass,
  tuneLength = 10, # tries 10 diff values f/each tuning parameter in model.
  trControl = myControl
  # tuning parameters: intercept only. No tuneGrid needed.
  )
olsmodel # prints model to console

### test the OLS regression model
results_olsmodel <- olsmodel$results # results: 362.8577 RMSE
olsmodel$bestTune # result: intercept = TRUE
(olsmodel_cv_rsq <- olsmodel$results$Rsquared)

### predict on holdout set (test_tbl)
p_olsmodel <- predict(olsmodel, test_tbl, na.action=na.pass) 
error_olsmodel <- p_olsmodel-test_tbl[["work_hours"]]
RMSE_olsmodel <- sqrt(mean(error_olsmodel^2)) # 718.7421 RMSE
  ## Test set RMSE is higher than the training set RMSE because we overfit the 
  ## training set, and the test set contains data the model hasn't seen before.
(olsmodel_ho_rsq <- cor(p_olsmodel, test_tbl$work_hours)^2)

### predict on full gss_tbl
p_olsmodel.f <- predict(olsmodel, gss_tbl, na.action=na.pass)
error_olsmodel.f <- p_olsmodel.f-gss_tbl[["work_hours"]]
RMSE_olsmodel.f <- sqrt(mean(error_olsmodel.f^2)) # 358.74 RMSE
(olsmodel_f_rsq <- cor(p_olsmodel.f, gss_tbl$work_hours)^2)


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
  ## tuning parameters: alpha, lambda.
  tuneGrid = expand.grid(
    alpha = 0:1, 
    lambda = seq(0.0001, 1, length = 20))
  )
enetmodel # prints model to console

### test the elastic net model
results_enetmodel <- enetmodel$results
enetmodel$bestTune # result: alpha = 1, lambda = 0.7368684
(enetmodel_cv_rsq <- max(enetmodel$results$Rsquared))

### predict on holdout set (test_tbl)
p_enetmodel <- predict(enetmodel, test_tbl, na.action=na.pass)
error_enetmodel <- p_enetmodel-test_tbl[["work_hours"]]
RMSE_enetmodel <- sqrt(mean(error_enetmodel^2)) # 13.17391
(enetmodel_ho_rsq <- cor(p_enetmodel, test_tbl$work_hours)^2)

### predict on full gss_tbl
p_enetmodel.f <- predict(enetmodel, gss_tbl, na.action=na.pass)
error_enetmodel.f <- p_enetmodel.f-gss_tbl[["work_hours"]]
RMSE_enetmodel.f <- sqrt(mean(error_enetmodel.f^2)) # 10.28183
(emetmodel_f_rsq <- cor(p_enetmodel.f, gss_tbl$work_hours)^2)

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
  ## tuning parameters: mtry, splitrule, min.node.size.
  tuneGrid = data.frame(
    mtry = c(2, 3, 4, 5, 10, 20, 50, 100),
    splitrule = c("variance"),
    min.node.size = 5)
  )
rfmodel # prints model to console
plot(rfmodel) # plot model

### test the random forest model
results_rfmodel <- rfmodel$results
rfmodel$bestTune # result: mtry = 100
(rfmodel_cv_rsq <- max(rfmodel$results$Rsquared))

### predict on holdout set (test_tbl)
p_rfmodel <- predict(rfmodel, test_tbl, na.action=na.pass)
error_rfmodel <- p_rfmodel-test_tbl[["work_hours"]]
RMSE_rfmodel <- sqrt(mean(error_rfmodel^2)) # 13.80708
(rfmodel_ho_rsq <- cor(p_rfmodel, test_tbl$work_hours)^2)

### predict on full gss_tbl
p_rfmodel.f <- predict(rfmodel, gss_tbl, na.action=na.pass)
error_rfmodel.f <- p_rfmodel.f-test_tbl[["work_hours"]]
RMSE_rfmodel.f <- sqrt(mean(error_rfmodel.f^2)) # 23.13059
(rfmodel_f_rsq <- cor(p_rfmodel.f, gss_tbl$work_hours)^2)


## run the “eXtreme Gradient Boosting” model, "xgbDART"
set.seed(8712) # so I get reproducible results
xgbmodel <- train(
  work_hours ~ .,
  train_tbl,
  method = "xgbDART", # or "xgbTREE"
  preProcess = myProcess,
  na.action = na.pass,
  tuneLength = 10, # tries 10 diff values f/each tuning parameter in model.
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
xgbmodel # prints model to console
plot(xgbmodel) # plot model

### test the "eXtreme Gradient Boosting" model
xgbmodel$results
xgbmodel$bestTune # results: nrounds = 10, max_depth = 2, eta = 0.44, m_c_w = 3
(xgbmodel_cv_rsq <- max(xgbmodel$results$Rsquared))

### predict on holdout set (test_tbl)
p_xgbmodel <- predict(xgbmodel, test_tbl, na.action=na.pass)
error_xgbmodel <- p_xgbmodel-test_tbl[["work_hours"]]
RMSE_xgbmodel <- sqrt(mean(error_xgbmodel^2)) # 22.37695
(xgbmodel_ho_rsq <- cor(p_xgbmodel, test_tbl$work_hours)^2)

### predict on full gss_tbl
p_xgbmodel.f <- predict(xgbmodel, gss_tbl, na.action=na.pass)
error_xgbmodel.f <- p_xgbmodel.f-test_tbl[["work_hours"]]
RMSE_xgbmodel.f <- sqrt(mean(error_xgbmodel.f^2)) # 22.37695
(xgbmodel_f_rsq <- cor(p_xgbmodel.f, gss_tbl$work_hours)^2)


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
  cv_rsq = c(
    str_remove(formatC(olsmodel_cv_rsq, format = 'f', digits = 2), "^0",),
    str_remove(formatC(enetmodel_cv_rsq, format = 'f', digits = 2), "^0",),
    str_remove(formatC(rfmodel_cv_rsq, format = 'f', digits = 2), "^0",),
    str_remove(formatC(xgbmodel_cv_rsq, format = 'f', digits = 2), "^0",)),
  ho_rsq = c(olsmodel_ho_rsq, 
             enetmodel_ho_rsq, 
             rfmodel_ho_rsq, 
             xgbmodel_ho_rsq)
  )
table1_tbl

str_remove(formatC(analysis$p.value, format='f', digits=2), "^0")

## Round all values to 2 decimal places in this tibble and display them without leading zeros and to the hundredths place
  
# 1. How did your results change between models? Why do you think this happened, specifically?

# 2. How did your results change between k-fold CV and holdout CV? Why do you think this happened, specifically?

# 3. Among the four models, which would you choose for a real-life prediction problem, and why? Are there tradeoffs? Write up to a paragraph.
