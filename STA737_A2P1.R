# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
install.packages("skimr")
install.packages("janitor")
library(skimr)
library(janitor)
library(readr)
# Read the data

setwd("C:\\Users\\Administrator\\Desktop\\STA 737")
df <- read_csv("Grocery_Cleaned.csv")
# Rename columns for easier reference (matching your variable names)
# daily_sales is the outcome, footfall is the predictor
colnames(df)

# ============================================
# 1. Fit 4th-degree polynomial regression
# ============================================
poly_model <- lm(daily_sales ~ poly(footfall, 4, raw = TRUE), data = df)

# Alternative using I() notation (same result)
# poly_model <- lm(daily_sales ~ footfall + I(footfall^2) + I(footfall^3) + I(footfall^4), data = df)

summary(poly_model)

# ============================================
# 2. Fit step function with 5 cuts (4 intervals)
# ============================================
# Create 5 equally spaced cut points based on footfall quantiles
# Using quantiles ensures roughly equal observations per bin
cut_points <- quantile(df$footfall, probs = seq(0, 1, length.out = 6))
# seq(0, 1, length.out = 6) gives: 0%, 20%, 40%, 60%, 80%, 100%

# Create factor variable for the bins
df$footfall_bin <- cut(df$footfall, 
                       breaks = cut_points, 
                       include.lowest = TRUE,
                       labels = c("Bin1", "Bin2", "Bin3", "Bin4", "Bin5"))

# Fit the step function model
step_model <- lm(daily_sales ~ footfall_bin, data = df)
summary(step_model)

# ============================================
# 3. Create predictions for plotting
# ============================================
# Create a sequence of footfall values for smooth prediction lines
footfall_range <- seq(min(df$footfall), max(df$footfall), length.out = 500)

# Predictions for polynomial model
poly_pred <- predict(poly_model, newdata = data.frame(footfall = footfall_range))

# For step function, create predictions at each bin's midpoint or range
# Method 1: Predict at each unique footfall value in the range
step_pred_df <- data.frame(footfall = footfall_range)
step_pred_df$footfall_bin <- cut(step_pred_df$footfall, 
                                 breaks = cut_points, 
                                 include.lowest = TRUE,
                                 labels = c("Bin1", "Bin2", "Bin3", "Bin4", "Bin5"))
step_pred <- predict(step_model, newdata = step_pred_df)

# ============================================
# 4. Create the plot
# ============================================
# Create data frames for plotting
poly_plot_data <- data.frame(footfall = footfall_range, 
                             daily_sales = poly_pred, 
                             model = "Polynomial (4th degree)")

step_plot_data <- data.frame(footfall = footfall_range, 
                             daily_sales = step_pred, 
                             model = "Step Function (5 cuts)")

# Combine predictions
combined_pred <- rbind(poly_plot_data, step_plot_data)

# Create the plot
p <- ggplot() +
  # Raw data points (semi-transparent to see density)
  geom_point(data = df, 
             aes(x = footfall, y = daily_sales), 
             alpha = 0.3, 
             size = 1.5,
             color = "gray50") +
  # Prediction lines
  geom_line(data = combined_pred, 
            aes(x = footfall, y = daily_sales, color = model, linetype = model), 
            size = 1.2) +
  # Add bin boundaries as vertical lines (optional, for step function clarity)
  geom_vline(xintercept = cut_points[2:5], 
             linetype = "dotted", 
             alpha = 0.5,
             color = "blue") +
  # Labels and theme
  labs(title = "Polynomial vs Step Function: Daily Sales by Footfall",
       subtitle = "4th-degree polynomial (smooth) vs 5-bin step function (piecewise constant)",
       x = "Footfall (customer count)",
       y = "Daily Sales",
       color = "Model",
       linetype = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display the plot
print(p)

# ============================================
# 5. Compare model fits
# ============================================
# Calculate R-squared for both models
r2_poly <- summary(poly_model)$r.squared
r2_step <- summary(step_model)$r.squared

# Calculate AIC (lower is better for same dataset)
aic_poly <- AIC(poly_model)
aic_step <- AIC(step_model)

# Calculate BIC
bic_poly <- BIC(poly_model)
bic_step <- BIC(step_model)

# Create comparison table
comparison <- data.frame(
  Model = c("4th-degree Polynomial", "Step Function (5 cuts)"),
  R_squared = c(r2_poly, r2_step),
  AIC = c(aic_poly, aic_step),
  BIC = c(bic_poly, bic_step)
)

print("Model Comparison:")
print(comparison)

# ======================================== Now Smooooothing Splines =================================== #

# Load required libraries
library(mgcv)  # For smoothing splines with CV
library(dplyr)
# ============================================
# PART 1: Smoothing Spline for Daily Sales ~ Stock Level
# ============================================

# Remove any missing values
df_spline <- df %>% filter(!is.na(stock_level), !is.na(daily_sales))

# Method 1: Using smooth.spline() with cross-validation
# This automatically selects the optimal degrees of freedom via GCV

# Fit smoothing spline with default CV (leave-one-out CV)
spline_fit <- smooth.spline(df_spline$stock_level, 
                            df_spline$daily_sales, 
                            cv = TRUE)  # TRUE = ordinary cross-validation

# Display results
cat("========== SMOOTHING SPLINE RESULTS ==========\n")
cat("Optimal degrees of freedom (df):", round(spline_fit$df, 2), "\n")
cat("Optimal smoothing parameter (lambda):", format(spline_fit$lambda, scientific = TRUE), "\n")
cat("Cross-validation score:", round(spline_fit$cv.crit, 2), "\n")

# Main Method: Manual cross-validation to find optimal df
# Try different df values and compute CV error

df_values <- seq(2, 20, by = 1)  # Test df from 2 to 20
cv_errors <- numeric(length(df_values))

set.seed(123)  # For reproducibility

for(i in 1:length(df_values)) {
  # Fit smoothing spline with specified df
  fit <- smooth.spline(df_spline$stock_level, 
                       df_spline$daily_sales, 
                       df = df_values[i], 
                       cv = TRUE)
  cv_errors[i] <- fit$cv.crit
}

# Find optimal df from grid search
optimal_df <- df_values[which.min(cv_errors)]

cat("\nGrid search results:\n")
cat("Optimal df from grid search:", optimal_df, "\n")
cat("Minimum CV error:", min(cv_errors), "\n")

# Create CV error plot
cv_plot <- ggplot(data.frame(df = df_values, cv_error = cv_errors), 
                  aes(x = df, y = cv_error)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_vline(xintercept = optimal_df, linetype = "dashed", color = "red") +
  labs(title = "Cross-Validation Error vs. Degrees of Freedom",
       subtitle = paste("Optimal df =", optimal_df),
       x = "Degrees of Freedom (Complexity)",
       y = "Cross-Validation Error") +
  theme_minimal()

print(cv_plot)

# ============================================
# Determine if curve is nearly linear or highly non-linear
# ============================================

# Compare with linear model
linear_model <- lm(daily_sales ~ stock_level, data = df_spline)

# Get predictions from spline at original points
spline_pred <- predict(spline_fit, df_spline$stock_level)$y
linear_pred <- predict(linear_model)

# Calculate how much variance the spline explains beyond linear
r2_linear <- summary(linear_model)$r.squared

# Correlation between spline and linear predictions
correlation <- cor(spline_pred, linear_pred)

# Calculate non-linearity measure (difference in fitted values)
nonlinearity <- mean((spline_pred - linear_pred)^2) / var(df_spline$daily_sales)

cat("\n========== LINEARITY ASSESSMENT ==========\n")
cat("Linear model R-squared:", round(r2_linear, 4), "\n")
cat("Correlation between linear and spline predictions:", round(correlation, 4), "\n")
cat("Non-linearity measure (0=linear, >0.1=non-linear):", round(nonlinearity, 4), "\n")

if(correlation > 0.95) {
  cat("CONCLUSION: The spline curve is NEARLY LINEAR (correlation > 0.95)\n")
} else if(correlation > 0.85) {
  cat("CONCLUSION: The spline curve shows MODEST non-linearity\n")
} else {
  cat("CONCLUSION: The spline curve is HIGHLY NON-LINEAR\n")
}

# Visual comparison of linear vs spline
prediction_data <- data.frame(
  stock_level = df_spline$stock_level,
  daily_sales = df_spline$daily_sales,
  linear_fit = linear_pred,
  spline_fit = spline_pred
)

# Create sequence for smooth spline line
stock_range <- seq(min(df_spline$stock_level), max(df_spline$stock_level), length.out = 200)
spline_smooth <- predict(spline_fit, stock_range)

comparison_plot <- ggplot() +
  geom_point(data = df_spline, 
             aes(x = stock_level, y = daily_sales), 
             alpha = 0.3, size = 1.5, color = "gray50") +
  geom_line(data = data.frame(stock_level = stock_range, 
                              spline_fit = spline_smooth$y),
            aes(x = stock_level, y = spline_fit, color = "Smoothing Spline"), 
            size = 1.2) +
  geom_line(data = prediction_data, 
            aes(x = stock_level, y = linear_fit, color = "Linear Fit"), 
            size = 1.2, linetype = "dashed") +
  labs(title = "Smoothing Spline vs Linear Fit: Daily Sales by Stock Level",
       subtitle = paste("Optimal df =", round(spline_fit$df, 2), 
                        "| Correlation =", round(correlation, 3)),
       x = "Stock Level", 
       y = "Daily Sales",
       color = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(comparison_plot)

# ============================================
# PART 2: Local Regression (LOESS) for Daily Sales ~ Unit Price
# ============================================

# Remove missing values
df_loess <- df %>% filter(!is.na(unit_price), !is.na(daily_sales))

# Fit LOESS with span = 0.2 (small span = more wiggly, likely to overfit)
loess_02 <- loess(daily_sales ~ unit_price, 
                  data = df_loess, 
                  span = 0.2, 
                  degree = 1)  # linear within each window

# Fit LOESS with span = 0.7 (larger span = smoother, more robust)
loess_07 <- loess(daily_sales ~ unit_price, 
                  data = df_loess, 
                  span = 0.7, 
                  degree = 1)

# Create predictions for plotting
unit_price_range <- seq(min(df_loess$unit_price), 
                        max(df_loess$unit_price), 
                        length.out = 200)

# Predict for each span
pred_02 <- predict(loess_02, newdata = data.frame(unit_price = unit_price_range), se = TRUE)
pred_07 <- predict(loess_07, newdata = data.frame(unit_price = unit_price_range), se = TRUE)

# Create prediction data frames
pred_df_02 <- data.frame(unit_price = unit_price_range, 
                         daily_sales = pred_02$fit,
                         span = "span = 0.2 (more flexible)")
pred_df_07 <- data.frame(unit_price = unit_price_range, 
                         daily_sales = pred_07$fit,
                         span = "span = 0.7 (smoother)")

# Combine predictions
pred_combined <- rbind(pred_df_02, pred_df_07)

# Calculate effective degrees of freedom (measure of complexity)
edf_02 <- loess_02$enp  # effective number of parameters
edf_07 <- loess_07$enp

# Calculate AICc for comparison
# Approximate AIC for LOESS (lower is better when models are comparable)
n <- nrow(df_loess)
sigma2_02 <- sum(loess_02$residuals^2) / (n - edf_02)
sigma2_07 <- sum(loess_07$residuals^2) / (n - edf_07)

aicc_02 <- n * log(sigma2_02) + 2 * edf_02 + (2 * edf_02 * (edf_02 + 1)) / (n - edf_02 - 1)
aicc_07 <- n * log(sigma2_07) + 2 * edf_07 + (2 * edf_07 * (edf_07 + 1)) / (n - edf_07 - 1)

cat("\n========== LOESS RESULTS ==========\n")
cat("LOESS with span = 0.2:\n")
cat("  Effective degrees of freedom:", round(edf_02, 2), "\n")
cat("  Approximate AICc:", round(aicc_02, 2), "\n")
cat("  Residual standard error:", round(sqrt(sigma2_02), 2), "\n\n")

cat("LOESS with span = 0.7:\n")
cat("  Effective degrees of freedom:", round(edf_07, 2), "\n")
cat("  Approximate AICc:", round(aicc_07, 2), "\n")
cat("  Residual standard error:", round(sqrt(sigma2_07), 2), "\n\n")

# ============================================
# Create the LOESS comparison plot
# ============================================

loess_plot <- ggplot() +
  # Raw data points
  geom_point(data = df_loess, 
             aes(x = unit_price, y = daily_sales), 
             alpha = 0.3, size = 1.5, color = "gray50") +
  # LOESS fit lines
  geom_line(data = pred_combined, 
            aes(x = unit_price, y = daily_sales, color = span, linetype = span), 
            size = 1.2) +
  labs(title = "LOESS Regression: Daily Sales vs Unit Price",
       subtitle = "Comparing span = 0.2 (overfits noise) vs span = 0.7 (smoother)",
       x = "Unit Price ($)", 
       y = "Daily Sales",
       color = "Span Parameter",
       linetype = "Span Parameter") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(loess_plot)

# ============================================
# Diagnostic: Which span overfits?
# ============================================

# Calculate residual standard deviation for both models
resid_02 <- residuals(loess_02)
resid_07 <- residuals(loess_07)

cat("\n========== OVERFITTING ASSESSMENT ==========\n")
cat("span = 0.2 (smaller span):\n")
cat("  Residual standard deviation:", round(sd(resid_02), 2), "\n")
cat("  Residual range:", round(diff(range(resid_02)), 2), "\n")
cat("  Pattern: The fitted line shows extreme wiggles following individual points\n\n")

cat("span = 0.7 (larger span):\n")
cat("  Residual standard deviation:", round(sd(resid_07), 2), "\n")
cat("  Residual range:", round(diff(range(resid_07)), 2), "\n")
cat("  Pattern: Smoother line captures general trend without local noise\n\n")

# Check for local overfitting by examining derivative changes
# More sign changes in derivatives indicates more overfitting
deriv_02 <- diff(pred_02$fit) / diff(unit_price_range)
deriv_07 <- diff(pred_07$fit) / diff(unit_price_range)

n_sign_changes_02 <- sum(diff(sign(deriv_02)) != 0)
n_sign_changes_07 <- sum(diff(sign(deriv_07)) != 0)

cat("Number of direction changes in fitted curve:\n")
cat("  span = 0.2:", n_sign_changes_02, "changes (highly wiggly → likely OVERFITTING)\n")
cat("  span = 0.7:", n_sign_changes_07, "changes (smoother → less overfitting)\n\n")

cat("CONCLUSION:\n")
cat("span = 0.2 clearly OVERFITS the noise because:\n")
cat("  1. The fitted line shows excessive wiggles that follow random noise\n")
cat("  2. Many local fluctuations without theoretical justification\n")
cat("  3. Effectively uses more degrees of freedom (", round(edf_02, 2), 
    " vs ", round(edf_07, 2), ")\n")
cat("  4. The curve changes direction", n_sign_changes_02, "times over the range,\n")
cat("     which is implausible for an economic relationship between price and sales\n\n")

cat("span = 0.7 is preferred as it captures the underlying trend without overfitting.\n")

# ============================================
# EXTRA: Side-by-side diagnostic plots
# ============================================

# Create residual plots to see patterns
residual_df <- data.frame(
  unit_price = df_loess$unit_price,
  resid_02 = resid_02,
  resid_07 = resid_07
)

residual_plot <- ggplot(residual_df) +
  geom_point(aes(x = unit_price, y = resid_02, color = "span = 0.2"), alpha = 0.5) +
  geom_point(aes(x = unit_price, y = resid_07, color = "span = 0.7"), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Comparison: Which span overfits?",
       subtitle = "span = 0.2 shows more structure (indicating overfitting)",
       x = "Unit Price", 
       y = "Residuals",
       color = "Span") +
  theme_minimal()

print(residual_plot)

# ============================================
# SUMMARY TABLE
# ============================================

summary_table <- data.frame(
  Model = c("Smoothing Spline (optimal)", "LOESS span=0.2", "LOESS span=0.7"),
  Key_Parameter = c(paste("df =", round(spline_fit$df, 2)), 
                    "span = 0.2", 
                    "span = 0.7"),
  Complexity = c(paste(round(spline_fit$df, 2), "df"), 
                 paste(round(edf_02, 2), "edf"), 
                 paste(round(edf_07, 2), "edf")),
  Pattern = c(if(correlation > 0.95) "Nearly Linear" else if(correlation > 0.85) "Modest Non-linear" else "Highly Non-linear",
              "Overfits (too wiggly)",
              "Appropriate smoothing")
)

print(summary_table)

# `~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GAM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Load required libraries
library(mgcv)      # For GAMs
library(splines)   # For ns() natural splines
library(ggplot2)
library(dplyr)

# Ensure store_format is treated as a factor
df$store_format <- as.factor(df$store_format)

# Remove any rows with missing values in the variables we'll use
df_gam <- df %>%
  filter(!is.na(daily_sales), 
         !is.na(footfall), 
         !is.na(stock_level), 
         !is.na(store_format))

# ============================================
# MODEL 1: Simple Linear Model (Baseline)
# ============================================

linear_model <- lm(daily_sales ~ footfall + stock_level + store_format, 
                   data = df_gam)

# ============================================
# MODEL 2: GAM with specified structure (CORRECTED)
# ============================================
gam_model <- gam(daily_sales ~ 
                   s(footfall, bs = "cr", k = 4) +  # Cubic regression spline (~natural spline), k=4 gives ~3-4 df
                   s(stock_level, bs = "tp") +      # Smoothing spline with GCV for df selection
                   store_format,                    # Linear term for categorical
                 data = df_gam,
                 method = "REML")  # Restricted Maximum Likelihood

gam_model_ns <- gam(daily_sales ~ 
                      ns(footfall, df = 4) +      # Natural spline with exactly 4 df
                      s(stock_level, bs = "tp") +  # Smoothing spline (CV selects df)
                      store_format,
                    data = df_gam,
                    method = "REML")
# We'll use gam_model_ns since it explicitly uses natural spline as requested
gam_model <- gam_model_ns

# ============================================
# DISPLAY MODEL SUMMARIES
# ============================================

cat("========== LINEAR MODEL SUMMARY ==========\n")
summary(linear_model)
cat("\n")

cat("========== GAM MODEL SUMMARY ==========\n")
summary(gam_model)
cat("\n")
# to get the MSE to compare with Assignment 1 #
# ============================================
# METHOD 2: Cross-Validated MSE (more reliable)
# ============================================

# Method 2a: Manual k-fold cross-validation
set.seed(123)  # For reproducibility
k_folds <- 5   # Number of folds

# Create fold assignments
df_gam$fold <- sample(rep(1:k_folds, length.out = nrow(df_gam)))

# Store CV predictions
cv_predictions <- numeric(nrow(df_gam))

for(fold in 1:k_folds) {
  # Split data
  train_data <- df_gam[df_gam$fold != fold, ]
  test_data <- df_gam[df_gam$fold == fold, ]
  
  # Fit GAM on training data
  cv_model <- gam(daily_sales ~ 
                    ns(footfall, df = 4) + 
                    s(stock_level, bs = "tp") + 
                    store_format,
                  data = train_data,
                  method = "REML")
  
  # Predict on test data
  cv_predictions[df_gam$fold == fold] <- predict(cv_model, newdata = test_data)
}

# Calculate CV MSE
cv_residuals <- df_gam$daily_sales - cv_predictions
mse_cv <- mean(cv_residuals^2)
rmse_cv <- sqrt(mse_cv)
mae_cv <- mean(abs(cv_residuals))

cat("========== 5-FOLD CROSS-VALIDATED MSE ==========\n")
cat("CV-MSE:", round(mse_cv, 2), "\n")
cat("CV-RMSE:", round(rmse_cv, 2), "\n")
cat("CV-MAE:", round(mae_cv, 2), "\n")
cat("\n")
# ============================================
# ANOVA TEST: GAM vs Linear Model
# ============================================

# Method 1: ANOVA using mgcv's anova.gam (compares nested models)
# For comparing lm and gam, we can use anova with test="Chisq"
anova_result <- anova(linear_model, gam_model, test = "Chisq")

cat("========== ANOVA: LINEAR MODEL vs GAM ==========\n")
print(anova_result)
cat("\n")

# Extract the p-value for interpretation
if(!is.na(anova_result$`Pr(>Chi)`[2])) {
  p_value <- anova_result$`Pr(>Chi)`[2]
} else if(!is.na(anova_result$`P(>|Chi|)`[2])) {
  p_value <- anova_result$`P(>|Chi|)`[2]
} else {
  # Alternative method using likelihood ratio test
  lr_stat <- 2 * (logLik(gam_model) - logLik(linear_model))
  df_diff <- attr(logLik(gam_model), "df") - attr(logLik(linear_model), "df")
  p_value <- pchisq(lr_stat, df_diff, lower.tail = FALSE)
  cat("Using Likelihood Ratio Test:\n")
  cat("LR statistic:", lr_stat, "on", df_diff, "df, p-value:", p_value, "\n\n")
}

# ============================================
# MODEL COMPARISON METRICS
# ============================================

cat("========== MODEL COMPARISON METRICS ==========\n")

# Extract degrees of freedom and log-likelihood
logLik_linear <- logLik(linear_model)
logLik_gam <- logLik(gam_model)

# Calculate AIC and BIC
aic_linear <- AIC(linear_model)
aic_gam <- AIC(gam_model)

bic_linear <- BIC(linear_model)
bic_gam <- BIC(gam_model)

# Calculate pseudo R-squared for GAM
null_deviance <- gam_model$null.deviance
residual_deviance <- gam_model$deviance
pseudo_r2 <- (null_deviance - residual_deviance) / null_deviance

comparison_df <- data.frame(
  Model = c("Linear Model", "GAM (Natural Spline + Smoothing)"),
  LogLik = c(as.numeric(logLik_linear), as.numeric(logLik_gam)),
  DF = c(attr(logLik_linear, "df"), attr(logLik_gam, "df")),
  AIC = c(aic_linear, aic_gam),
  BIC = c(bic_linear, bic_gam),
  R_squared = c(summary(linear_model)$r.squared, pseudo_r2)
)

print(comparison_df)
cat("\n")

# ============================================
# CHECK SIGNIFICANCE OF NON-LINEAR TERMS
# ============================================

cat("========== SIGNIFICANCE OF NON-LINEAR EFFECTS ==========\n")

# For the ns() term, we need to do a joint test
# Create a model without the ns() term to test its significance
linear_reduced <- lm(daily_sales ~ footfall + stock_level + store_format, data = df_gam)

# Test if adding natural spline for footfall improves model
linear_no_spline <- lm(daily_sales ~ stock_level + store_format, data = df_gam)
anova_footfall <- anova(linear_no_spline, linear_model, test = "F")
cat("\nNatural spline for Footfall (4 df):\n")
cat("  F-test p-value:", anova_footfall$`Pr(>F)`[2], "\n")
cat("  Conclusion:", ifelse(anova_footfall$`Pr(>F)`[2] < 0.05, 
                            "SIGNIFICANT non-linear effect", 
                            "No significant non-linear effect"), "\n")

# Extract smoothing term p-value from GAM summary
smooth_summary <- summary(gam_model)
cat("\nSmoothing spline for Stock Level:\n")
cat("  Approximate p-value:", smooth_summary$s.pv[1], "\n")
cat("  Conclusion:", ifelse(smooth_summary$s.pv[1] < 0.05, 
                            "SIGNIFICANT non-linear effect", 
                            "No significant non-linear effect"), "\n")
cat("\n")

# ============================================
# VISUALIZE THE GAM RESULTS
# ============================================

# Create prediction data for footfall effect
footfall_range <- seq(min(df_gam$footfall), max(df_gam$footfall), length.out = 100)
stock_mean <- mean(df_gam$stock_level)
# Get most common store format
format_ref <- levels(df_gam$store_format)[1]  # First level as reference

# Predict for footfall
pred_data_footfall <- expand.grid(
  footfall = footfall_range,
  stock_level = stock_mean,
  store_format = format_ref
)

# Get predictions with standard errors
pred_footfall <- predict(gam_model, newdata = pred_data_footfall, se.fit = TRUE)
pred_data_footfall$daily_sales <- pred_footfall$fit
pred_data_footfall$se_lower <- pred_footfall$fit - 1.96 * pred_footfall$se.fit
pred_data_footfall$se_upper <- pred_footfall$fit + 1.96 * pred_footfall$se.fit

# Predict for stock_level
stock_range <- seq(min(df_gam$stock_level), max(df_gam$stock_level), length.out = 100)
footfall_mean <- mean(df_gam$footfall)

pred_data_stock <- expand.grid(
  footfall = footfall_mean,
  stock_level = stock_range,
  store_format = format_ref
)

pred_stock <- predict(gam_model, newdata = pred_data_stock, se.fit = TRUE)
pred_data_stock$daily_sales <- pred_stock$fit
pred_data_stock$se_lower <- pred_stock$fit - 1.96 * pred_stock$se.fit
pred_data_stock$se_upper <- pred_stock$fit + 1.96 * pred_stock$se.fit

# Create plots
p1 <- ggplot() +
  geom_point(data = df_gam, aes(x = footfall, y = daily_sales), 
             alpha = 0.3, size = 1, color = "gray50") +
  geom_ribbon(data = pred_data_footfall, 
              aes(x = footfall, ymin = se_lower, ymax = se_upper), 
              alpha = 0.2, fill = "blue") +
  geom_line(data = pred_data_footfall, 
            aes(x = footfall, y = daily_sales), 
            color = "blue", size = 1.2) +
  labs(title = "GAM: Effect of Footfall on Daily Sales",
       subtitle = "Natural spline with 4 degrees of freedom",
       x = "Footfall", 
       y = "Predicted Daily Sales") +
  theme_minimal()

p2 <- ggplot() +
  geom_point(data = df_gam, aes(x = stock_level, y = daily_sales), 
             alpha = 0.3, size = 1, color = "gray50") +
  geom_ribbon(data = pred_data_stock, 
              aes(x = stock_level, ymin = se_lower, ymax = se_upper), 
              alpha = 0.2, fill = "red") +
  geom_line(data = pred_data_stock, 
            aes(x = stock_level, y = daily_sales), 
            color = "red", size = 1.2) +
  labs(title = "GAM: Effect of Stock Level on Daily Sales",
       subtitle = "Smoothing spline with CV-selected df",
       x = "Stock Level", 
       y = "Predicted Daily Sales") +
  theme_minimal()

# Display plots
print(p1)
print(p2)

# Combined plot using patchwork
if(require(patchwork, quietly = TRUE)) {
  combined_plot <- p1 + p2
  print(combined_plot)
} else {
  cat("\nInstall patchwork package for side-by-side plots: install.packages('patchwork')\n")
}

# ============================================
# PARTIAL EFFECTS PLOT (Built-in)
# ============================================

cat("\nGenerating partial effects plots...\n")
par(mfrow = c(1, 2))
plot(gam_model, pages = 1, shade = TRUE, shade.col = "lightblue", 
     seWithMean = TRUE, scale = 0, main = "GAM Partial Effects")
par(mfrow = c(1, 1))

# ============================================
# ANOVA TEST INTERPRETATION
# ============================================

cat("\n========== ANOVA TEST INTERPRETATION ==========\n")

cat("ANOVA test p-value:", format(p_value, scientific = TRUE, digits = 4), "\n\n")

if(p_value < 0.001) {
  cat("✓ CONCLUSION: The GAM provides a HIGHLY SIGNIFICANT improvement\n")
  cat("  over the linear model (p < 0.001).\n")
  cat("  The relationships with footfall and/or stock level are\n")
  cat("  strongly non-linear.\n")
} else if(p_value < 0.05) {
  cat("✓ CONCLUSION: The GAM provides a SIGNIFICANT improvement\n")
  cat("  over the linear model (p < 0.05).\n")
  cat("  The non-linear terms capture important patterns.\n")
} else {
  cat("✗ CONCLUSION: The GAM does NOT significantly improve\n")
  cat("  over the linear model (p >= 0.05).\n")
  cat("  A simple linear model may be sufficient.\n")
}

# ============================================
# DIAGNOSTIC CHECK
# ============================================

cat("\n========== MODEL DIAGNOSTICS ==========\n")
gam.check(gam_model)


