# Script Settings and Resources
library(tidyverse)
library(haven)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data Import and Cleaning
data <- read_sav("../data/GSS2016.sav")
  ## 'user_na=FALSE' is the default. So, I didn't add it. It ensures any 
  ## missing, don’t know, inapplicable, or otherwise not-clearly-answered 
  ## items are marked as missing values.

gss_tbl <- data %>%
  mutate_all(~ifelse(.==0, NA, .)) %>%  # turns the 0s into NAs
  ##### the filter() and drop_na() code below are completely removing MOSTHRS
  # filter(!is.na(MOSTHRS)) %>%  # removes anyone with NA in MOSTHRS
  # drop_na(MOSTHRS) %>%
  rename(work_hours = MOSTHRS) %>%  # changes MOSTHRS label to work_hours
  select(-c(HRS1, HRS2)) %>%  # removes HRS1 and HRS2 variables
  select(-where(~ mean(is.na(.))<.75))  # retains only variables w/ < 75% NAs

# check data upload...
names(gss_tbl)
colMeans(is.na(gss_tbl)) < .75
View(gss_tbl$work_hours)  ## rather than removing the NAs in MOSTHRS, it is 
  ## completely removing the whole column :(

if (any(gss_tbl==0, na.rm = TRUE)) {
  print("There are 0s in the dataset.")
} else {
  print("There are no 0s in the dataset.")
}

if (any(gss_tbl$work_hours==NA, na.rm = TRUE)) {
  print("There are NAs in the dataset.")
} else {
  print("There are no NAs in the dataset.")
}

if (any(colMeans(is.na(gss_tbl)) < .75, na.rm = TRUE)) {
  print("There are < .75 in the dataset.")
} else {
  print("There are no < .75 in the dataset.")
}

# Visualization
## includes a visualization of the univariate distribution of work hours
gss_tbl %>%
  ggplot(aes(x=work_hours)) +
  geom_histogram(binwidth = 5, fill = "gray", color = "darkblue") +
  labs(title = "Distribution of Work Hours",
       x = "Work Hours",
       y = "Frequency") +
  theme_minimal()
  
# Analysis

  
# Publication
table1_tbl <- 
  # Round all values to 2 decimal places in this tibble and display them without leading zeros and to the hundredths place
  
# 1. How did your results change between models? Why do you think this happened, specifically?
# 2. How did your results change between k-fold CV and holdout CV? Why do you think this happened, specifically?
# 3. Among the four models, which would you choose for a real-life prediction problem, and why? Are there tradeoffs? Write up to a paragraph