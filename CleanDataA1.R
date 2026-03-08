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
# plot
plot(sales_model, which = 1)

leverage_values <- hatvalues(sales_model)
leverage_values

n <- nrow(train_data)
p <- length(coef(sales_model)) - 1

threshold <- 2 * (p + 1) / n
threshold

which(leverage_values > threshold)
sum(leverage_values > threshold)
summary(sales_model)

cooks_d <- cooks.distance(sales_model)

plot(cooks_d)
abline(h = 4/n, col="red")

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
class_model <- glm(
  inventory_status ~ unit_price + footfall +
    promotion_active + store_format,
  data = train_data,
  family = binomial
)

# Predict normally
probabilities <- predict(class_model,
                         newdata = test_data,
                         type = "response")

predicted_class <- ifelse(probabilities > 0.3,
                          "High_Priority",
                          "Low_Priority")

predicted_class <- factor(predicted_class,
                          levels = levels(test_data$inventory_status))

# las this line
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

# Calculating MSE for train and test data
# fit model on training data
reg_model <- lm(daily_sales ~ footfall + unit_price + 
                  promotion_active + store_format,
                data = train_data)

# Predict on Training and Test Sets
train_pred <- predict(reg_model, newdata = train_data)
test_pred  <- predict(reg_model, newdata = test_data)

# Compute MSE
train_mse <- mean((train_data$daily_sales - train_pred)^2)
test_mse  <- mean((test_data$daily_sales - test_pred)^2)

train_mse
test_mse

### Run a 10-fold Cross-Validation on the Regression Model
# Install and Load the cross-validation library
install.packages("boot")   # run once
library(boot)

# Regression Model for CV
cv_model <- glm(
  daily_sales ~ footfall + unit_price + promotion_active + store_format,
  data = grocery_clean
)  

# Run 10-Fold Cross-Validation
set.seed(123)

cv_results <- cv.glm(
  data = grocery_clean,
  glmfit = cv_model,
  K = 10
)

# Extract the Cross-Validated Error
cv_results$delta
cv_mse <- cv_results$delta[1]

# to compare from phase II
train_mse
test_mse
cv_mse

###------------------Phase V: Bootstrap-----------------------###

# Bootstrap with 1,000 samples of size 1,000 to find standard errors for Footfall and Unit_Price

# Load required library
library(boot)

# Manual bootstrap approach (without boot package)
set.seed(123)  # For reproducibility
n_bootstrap <- 1000
sample_size <- 1000
n_total <- nrow(grocery_clean)

# Adjust sample size if dataset is smaller
if(sample_size > n_total) {
  warning("Sample size (1000) is larger than dataset. Using dataset size (", n_total, ") instead.")
  sample_size <- n_total
}

# Initialize vectors to store coefficients
footfall_coefs <- numeric(n_bootstrap)
unit_price_coefs <- numeric(n_bootstrap)

# Run bootstrap manually
for(i in 1:n_bootstrap) {
  # Sample with replacement
  boot_indices <- sample(1:n_total, size = sample_size, replace = TRUE)
  boot_data <- grocery_clean[boot_indices, ]
  
  # Fit model on bootstrap sample
  boot_model <- lm(daily_sales ~ footfall + unit_price + promotion_active + store_format,
                   data = boot_data)
  
  # Store coefficients
  footfall_coefs[i] <- coef(boot_model)["footfall"]
  unit_price_coefs[i] <- coef(boot_model)["unit_price"]
}

# Calculate standard errors
footfall_se <- sd(footfall_coefs, na.rm = TRUE)
unit_price_se <- sd(unit_price_coefs, na.rm = TRUE)

# Display results
cat("Bootstrap Results (", n_bootstrap, " samples of size ", sample_size, "):\n", sep = "")
cat("========================================\n")
cat("Standard Error for Footfall:", round(footfall_se, 6), "\n")
cat("Standard Error for Unit_Price:", round(unit_price_se, 6), "\n\n")

# Display coefficient distributions
cat("Coefficient Summary Statistics:\n")
cat("Footfall - Mean:", round(mean(footfall_coefs, na.rm = TRUE), 6), 
    "| SE:", round(footfall_se, 6), "\n")
cat("Unit_Price - Mean:", round(mean(unit_price_coefs, na.rm = TRUE), 6), 
    "| SE:", round(unit_price_se, 6), "\n")

# 95% Confidence Intervals
cat("\n95% Bootstrap Confidence Intervals:\n")
cat("Footfall: [", round(quantile(footfall_coefs, 0.025, na.rm = TRUE), 6), 
    ",", round(quantile(footfall_coefs, 0.975, na.rm = TRUE), 6), "]\n")
cat("Unit_Price: [", round(quantile(unit_price_coefs, 0.025, na.rm = TRUE), 6), 
    ",", round(quantile(unit_price_coefs, 0.975, na.rm = TRUE), 6), "]\n")

# Quick histogram to visualize the distributions
par(mfrow = c(1, 2))
hist(footfall_coefs, breaks = 30, main = "Footfall Distribution",
     xlab = "Coefficient Value", col = "lightblue")
abline(v = mean(footfall_coefs), col = "red", lwd = 2)
hist(unit_price_coefs, breaks = 30, main = "Unit Price Distribution",
     xlab = "Coefficient Value", col = "lightgreen")
abline(v = mean(unit_price_coefs), col = "red", lwd = 2)


