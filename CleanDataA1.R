# Important libraries to load
library(tidyverse)
install.packages("skimr")
install.packages("janitor")
library(skimr)
library(janitor)
library(readr)
library(dplyr)

# then to load the data
grocery_data <- read_delim("C://Users//Administrator//Desktop//STA 737//Assignment 1_Grocery.csv", delim = NULL, trim_ws = TRUE)
glimpse(grocery_data)
names(grocery_data)

dim(grocery_data)
str(grocery_data)
# the line below was used to verify the seperator
# readLines("C://Users//Administrator//Desktop//STA 737//Assignment 1_Grocery.csv", n = 5)


# for the initial data inspection - structure and basic summary
summary(grocery_data)

# fix the negative value -5.990 becomes 5.990
grocery_data <- grocery_data %>%
  mutate(
    unit_price = abs(unit_price)
  )

# convert 4500 into NA before KNN
grocery_data <- grocery_data %>%
  mutate(
    daily_sales = ifelse(daily_sales == 4500, NA, daily_sales)
  )
# convert categorical variables to factors (important for KNN)
grocery_data <- grocery_data %>%
  mutate(
    inventory_status = factor(inventory_status),
    store_format = factor(store_format),
    promotion_active = factor(promotion_active)
  )
# convert the 0 im footfall to NA before imputation
grocery_data <- grocery_data %>%
  mutate(
    footfall = ifelse(footfall == 0, NA, footfall)
  )

# check missingness before imputation
colSums(is.na(grocery_data))

# library for KNN imputation
install.packages("VIM")
library(VIM)
# apply KNN Imputation (core step)
set.seed(123)  # reproducibility

grocery_knn <- kNN(
  grocery_data,
  k = 5,          # standard choice for small datasets
  imp_var = FALSE
)

# Final Validation
colSums(is.na(grocery_knn))
summary(grocery_knn)

# Make a copy of the dataset to keep the original intact
grocery_raw <- grocery_data
grocery_clean <- grocery_knn

summary(grocery_clean)

### Saving the new clean data set on my device
dim(grocery_clean)
write.csv(grocery_clean, 
          "Grocery_Cleaned.csv", 
          row.names = FALSE)
getwd()

# Excellent work brudda!

#Now we wanna make plots and graphs innit
# install and load visualising tools
install.packages("GGally")  # run once
library(GGally)

# scatter plot matrix with colour by promotion
ggpairs(
  grocery_clean,
  columns = c("daily_sales", "unit_price", "footfall", "stock_level"),
  aes(colour = promotion_active, alpha = 0.6),
  title = "Scatter Plot Matrix by Promotion Status"
)

# Alright, after the scatter plot we build linear regression, so we make the categorical ones numerical using dummy code
grocery_clean$promotion_active <- factor(grocery_clean$promotion_active)
grocery_clean$store_format <- factor(grocery_clean$store_format)

# Fit the Linear Regression Model
sales_model <- lm(
  daily_sales ~ inventory_status + unit_price + footfall + stock_level +
    promotion_active + store_format,
  data = grocery_clean
)

summary(sales_model)

# Model diagnostics - build residual plots to check for funny stuff
par(mfrow = c(2,2))
plot(sales_model)

# check for multicollinearity
install.packages("car")  # if needed
library(car)

vif(sales_model)

### phase III: Classification Strategy
# ensure correct data types

grocery_clean$inventory_status <- factor(grocery_clean$inventory_status)
grocery_clean$promotion_active <- factor(grocery_clean$promotion_active)
grocery_clean$store_format <- factor(grocery_clean$store_format)

levels(grocery_clean$inventory_status) # High_Priority is what we care about

# Train-Test Split data
set.seed(123)

train_index <- sample(1:nrow(grocery_clean), 
                      size = 0.7*nrow(grocery_clean))

train_data <- grocery_clean[train_index, ]
test_data  <- grocery_clean[-train_index, ]

# Fit Logistic Regression Model
class_model <- glm(
  inventory_status ~ unit_price + footfall + 
    promotion_active + store_format,
  data = train_data,
  family = binomial
)

summary(class_model)

# Generate Predictions
# Predicted probabilities
probabilities <- predict(class_model, 
                         newdata = test_data, 
                         type = "response")

# Convert to class predictions (0.5 threshold)
predicted_class <- ifelse(probabilities > 0.5,
                          "Low_Priority",
                          "High_Priority")

predicted_class <- factor(predicted_class,
                          levels = levels(test_data$inventory_status))

# Confusion Matrix
table(Predicted = predicted_class,
      Actual = test_data$inventory_status)

# Calculate Performance Metrics
conf_mat <- table(Predicted = predicted_class_03,
                  Actual = test_data$inventory_status)
conf_mat
table(predicted_class)
table(test_data$inventory_status)

accuracy <- sum(diag(conf_mat)) / sum(conf_mat)

sensitivity <- conf_mat["High_Priority","High_Priority"] /
  sum(conf_mat[,"High_Priority"])

specificity <- conf_mat["Low_Priority","Low_Priority"] /
  sum(conf_mat[,"Low_Priority"])

accuracy
sensitivity
specificity

# Adjust Threshold
grocery_clean$inventory_status <- relevel(
  grocery_clean$inventory_status,
  ref = "Low_Priority"
)

# Refit the logistic regression

predicted_class_03 <- ifelse(probabilities > 0.3,
                             "Low_Priority",
                             "High_Priority")
#------------------------------------------------------------------------------#
# checking for overlap
table(grocery_clean$inventory_status,
      grocery_clean$stock_level)

#is there still overlap?
summary(grocery_clean$stock_level[grocery_clean$inventory_status == "High_Priority"])
summary(grocery_clean$stock_level[grocery_clean$inventory_status == "Low_Priority"])