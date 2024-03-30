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
  select(-where(~mean(is.na(.))>.75)) %>%  # retains variables w/ < 75% NAs
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
## Loop over all four models at once
results <- list()
for (model_name in c("OLS Regression", "Elastic Net",
                     "Random Forest", "eXtreme Gradient Boosting")) {
  set.seed(8712) # for reproducibility
  
  # Define method for all models
  if (model_name == "OLS Regression") {
    myMethod <- "lm" 
  } else if (model_name == "Elastic Net") {
    myMethod <- "glmnet" 
  } else if (model_name == "Random Forest") {
    myMethod <- "ranger"
  } else if (model_name == "eXtreme Gradient Boosting") {
    myMethod <- "xgbTree"
  }
  
  # Define tuneGrid for all models
  if (model_name == "OLS Regression") {
    myGrid <- NULL # tuning parameters: intercept only. No tuneGrid needed.
  } else if (model_name == "Elastic Net") {
    myGrid <- expand.grid(
      alpha = c(0, 1), # 0=LASSO, 1=ridge
      lambda = seq(0.0001, 0.1, length = 20) # complexity penalties
    )
  } else if (model_name == "Random Forest") {
    myGrid <- data.frame( 
      mtry = c(2, 3, 4, 5, 10, 20, 50, 100), # tuneLength changes mtry
      splitrule = c("variance"),
      min.node.size = 5
    )
  } else if (model_name == "eXtreme Gradient Boosting") {
    myGrid <- expand.grid( 
      nrounds = c(2, 5, 10), # boosting iterations
      max_depth = c(2, 5, 10), # max tree depth
      eta = seq(0, 1, length = 10), # shrinkage
      gamma = 0, # minimum loss reduction
      colsample_bytree = 1, # subsample ratio of columns
      min_child_weight = c(1, 2, 3), # min sum of instance weight
      subsample = 1 # subsample percentage
    )
  }
  
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
    method = myMethod,
    preProcess = c("center", "scale", "medianImpute"), # imputes missing values
    na.action = na.pass,
    tuneLength = 10,
    trControl = trainControl(method = "cv", # cross-validation
                             number = 10, # 10 fold cv
                             search = "grid",
                             verboseIter = T), # prints additional details
    tuneGrid = myGrid
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
  cv_rsq = character(), # because I had to convert it to remove the leading 0
  ho_rsq = character() # because I had to convert it to remove the leading 0
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
### OLS Regression was a very poor fit. When comparing the output directly via 
# summary(resamples(list(model1, model2, model3, model4))), I even removed the 
# OLS model to get a better look at the other three since it was so much worse 
# than them. Clearly, using linear regression for this data would not be ideal 
# for making predictions. As for the other three models, the differences could 
# be due to model complexity, underlying assumptions, and hyperparameter tuning # (which could have been continually tweaked for a long time). Or, it could 
# just be a problem with the data quality and/or quantity.  

# 2. How did your results change between k-fold CV and holdout CV? Why do you think this happened, specifically?
### The R-squared decreased in the holdout CV because it was predicting on data # that it hadn't previously seen before. Additionally, the RMSE value was 
# significantly higher on the holdout CV (718.74 vs. 362.86) for "lm" because 
# the training set was likely overfit.

# 3. Among the four models, which would you choose for a real-life prediction problem, and why? Are there tradeoffs? Write up to a paragraph.
### Based on the R-squared values, I would use the eXtreme Gradient Boosting 
# for a real0life prediction problem based on this data. The pros for "xgbTree" # include: high performance, scalability, flexibility, and feature importance 
# scores showing which features are most influential in making predictions. 
# However, the cons include: complexity, sensitivity to hyperparameters, and 
# significant training time for large datasets and complex models. 
# Additionally, "xgbTree" can be considered a black-box model, where it is 
# difficult to understand and interpret the relationship between input features # and predictions. 
# Random Forest could also be a good model to select as it tends to have high 
# accuracy, robustness to overfitting, and an ability to handle missing values 
# and outlines. However, if we had used the full original dataset (including 
# the large quantity of missing values), it might have done worse since Random 
# Forest is less effective on sparse data. Otherwise, it has similar cons as 
# "xgbTree": black box model, bias in feature importance, and computational 
# resources.
