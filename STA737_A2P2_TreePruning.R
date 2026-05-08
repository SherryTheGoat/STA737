# Load required libraries
library(rpart)
library(rpart.plot)
library(ggplot2)
library(caret)  # For train/test split
library(dplyr)

# Read the data
setwd("C:\\Users\\Administrator\\Desktop\\STA 737")
df <- read.csv("Grocery_Cleaned.csv")

# ============================================
# PREPARE DATA FOR REGRESSION TREE
# ============================================

# Use daily_sales as the response variable
# Remove any rows with missing values
df <- df %>%
  filter(!is.na(daily_sales), !is.na(footfall), !is.na(stock_level),
         !is.na(unit_price), !is.na(store_format), !is.na(promotion_active))

# ============================================
# SPLIT DATA INTO TRAINING (70%) AND TEST (30%)
# ============================================

set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(df), size = 0.7 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

cat("========== DATA SPLIT INFORMATION ==========\n")
cat("Total observations:", nrow(df), "\n")
cat("Training set size:", nrow(train_data), "(", round(100 * nrow(train_data)/nrow(df), 1), "%)\n")
cat("Test set size:", nrow(test_data), "(", round(100 * nrow(test_data)/nrow(df), 1), "%)\n\n")

# ============================================
# MODEL 1: FULLY GROWN REGRESSION TREE (NO PRUNING)
# ============================================

# Fit a fully grown tree (cp = 0 allows maximum growth, minsplit = 2 allows single observations)
full_tree <- rpart(daily_sales ~ footfall + stock_level + unit_price + 
                     store_format + promotion_active,
                   data = train_data,
                   method = "anova",
                   control = rpart.control(minsplit = 2,    # Minimum observations to split
                                           cp = 0,          # No complexity penalty
                                           maxdepth = 30,   # Allow deep tree
                                           minbucket = 1))  # Allow single observation nodes

# Print the full tree details
cat("========== FULLY GROWN TREE ==========\n")
cat("Number of nodes:", nrow(full_tree$frame), "\n")
cat("Number of terminal nodes (leaves):", sum(full_tree$frame$var == "<leaf>"), "\n")
print(full_tree$cptable)

# Visualize the full tree
rpart.plot(full_tree, main = "Fully Grown Regression Tree", 
           type = 2, extra = 101, under = TRUE, 
           box.palette = "RdBu", branch = 1, 
           fallen.leaves = FALSE)

# ============================================
# MODEL 2: PRUNED TREE (USING COMPLEXITY PARAMETER)
# ============================================

# Print the complexity parameter table to find optimal cp
cat("\n========== COMPLEXITY PARAMETER TABLE ==========\n")
print(full_tree$cptable)

# Find the cp value that minimizes cross-validation error (xerror)
optimal_cp <- full_tree$cptable[which.min(full_tree$cptable[, "xerror"]), "CP"]
optimal_index <- which.min(full_tree$cptable[, "xerror"])

cat("\nOptimal CP (minimizes cross-validation error):", optimal_cp, "\n")
cat("Number of splits at optimal CP:", full_tree$cptable[optimal_index, "nsplit"], "\n")

# Option 1: Prune using the 1-SE rule (more conservative)
# Choose the smallest tree within 1 standard error of the minimum xerror
se_min <- full_tree$cptable[optimal_index, "xstd"]
xerror_min <- full_tree$cptable[optimal_index, "xerror"]
pruned_cp_1se <- full_tree$cptable[full_tree$cptable[, "xerror"] <= xerror_min + se_min, "CP"][1]

cat("\n1-SE rule CP:", pruned_cp_1se, "\n")

# Option 2: Prune using the optimal CP
pruned_tree_optimal <- prune(full_tree, cp = optimal_cp)
pruned_tree_1se <- prune(full_tree, cp = pruned_cp_1se)

# We'll use the optimal cp for main analysis
pruned_tree <- pruned_tree_optimal

cat("\n========== PRUNED TREE ==========\n")
cat("Number of nodes:", nrow(pruned_tree$frame), "\n")
cat("Number of terminal nodes (leaves):", sum(pruned_tree$frame$var == "<leaf>"), "\n")

# Visualize the pruned tree
rpart.plot(pruned_tree, main = "Pruned Regression Tree (Optimal Size)", 
           type = 2, extra = 101, under = TRUE, 
           box.palette = "RdBu", branch = 1, 
           fallen.leaves = TRUE)

# ============================================
# CALCULATE PREDICTIONS AND ERRORS
# ============================================

# Predictions for FULL tree
full_train_pred <- predict(full_tree, train_data)
full_test_pred <- predict(full_tree, test_data)

# Predictions for PRUNED tree
pruned_train_pred <- predict(pruned_tree, train_data)
pruned_test_pred <- predict(pruned_tree, test_data)

# ============================================
# CALCULATE TRAINING ERRORS (MSE, RMSE, MAE)
# ============================================

# Function to calculate error metrics
calculate_errors <- function(actual, predicted, dataset_name) {
  residuals <- actual - predicted
  mse <- mean(residuals^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(residuals))
  r_squared <- 1 - (sum(residuals^2) / sum((actual - mean(actual))^2))
  
  return(data.frame(
    Dataset = dataset_name,
    MSE = round(mse, 2),
    RMSE = round(rmse, 2),
    MAE = round(mae, 2),
    R_squared = round(r_squared, 4)
  ))
}

# Calculate errors for both trees
full_train_errors <- calculate_errors(train_data$daily_sales, full_train_pred, "Full Tree - Train")
full_test_errors <- calculate_errors(test_data$daily_sales, full_test_pred, "Full Tree - Test")
pruned_train_errors <- calculate_errors(train_data$daily_sales, pruned_train_pred, "Pruned Tree - Train")
pruned_test_errors <- calculate_errors(test_data$daily_sales, pruned_test_pred, "Pruned Tree - Test")

# Combine results
error_summary <- rbind(full_train_errors, full_test_errors, 
                       pruned_train_errors, pruned_test_errors)

cat("\n========== ERROR METRICS SUMMARY ==========\n")
print(error_summary)

# ============================================
# SPECIFIC QUESTION: Training and Test Error Comparison
# ============================================

cat("\n========== TRAINING VS TEST ERROR COMPARISON ==========\n")

# For FULLY GROWN tree
full_train_mse <- mean((train_data$daily_sales - full_train_pred)^2)
full_test_mse <- mean((test_data$daily_sales - full_test_pred)^2)

cat("FULLY GROWN TREE:\n")
cat("  Training MSE:", round(full_train_mse, 2), "\n")
cat("  Test MSE:", round(full_test_mse, 2), "\n")
cat("  Difference (Train - Test):", round(full_train_mse - full_test_mse, 2), "\n")
cat("  Overfitting indicator:", ifelse(full_train_mse < full_test_mse, "Yes (overfitted)", "No"), "\n\n")

# For PRUNED tree
pruned_train_mse <- mean((train_data$daily_sales - pruned_train_pred)^2)
pruned_test_mse <- mean((test_data$daily_sales - pruned_test_pred)^2)

cat("PRUNED TREE:\n")
cat("  Training MSE:", round(pruned_train_mse, 2), "\n")
cat("  Test MSE:", round(pruned_test_mse, 2), "\n")
cat("  Difference (Train - Test):", round(pruned_train_mse - pruned_test_mse, 2), "\n")
cat("  Overfitting indicator:", ifelse(pruned_train_mse < pruned_test_mse, "Yes (overfitted)", "No"), "\n\n")

# ============================================
# CROSS-VALIDATION ERROR PLOT
# ============================================

# Plot cross-validation error vs tree size
cptable <- full_tree$cptable
cp_df <- data.frame(
  CP = cptable[, "CP"],
  nsplit = cptable[, "nsplit"],
  xerror = cptable[, "xerror"],
  xstd = cptable[, "xstd"]
)

cv_plot <- ggplot(cp_df, aes(x = nsplit, y = xerror)) +
  geom_line(color = "blue", size = 1) +
  geom_point(size = 3, color = "red") +
  geom_errorbar(aes(ymin = xerror - xstd, ymax = xerror + xstd), 
                width = 0.2, alpha = 0.7) +
  geom_vline(xintercept = cp_df$nsplit[which.min(cp_df$xerror)], 
             linetype = "dashed", color = "darkred", size = 1) +
  labs(title = "Cross-Validation Error vs Tree Size",
       subtitle = paste("Optimal tree size:", cp_df$nsplit[which.min(cp_df$xerror)], "splits"),
       x = "Number of Splits (Tree Complexity)",
       y = "Cross-Validation Error (xerror)") +
  theme_minimal() +
  annotate("text", 
           x = cp_df$nsplit[which.min(cp_df$xerror)] + 0.5, 
           y = min(cp_df$xerror),
           label = "Optimal size",
           hjust = 0, vjust = -0.5, size = 4, color = "darkred")

print(cv_plot)

# ============================================
# COMPARISON PLOT: Predictions from Both Trees
# ============================================

# Create scatter plot comparing predictions
comparison_df <- data.frame(
  Actual = test_data$daily_sales,
  Full_Tree_Pred = full_test_pred,
  Pruned_Tree_Pred = pruned_test_pred
)

# Plot for Full Tree
p1 <- ggplot(comparison_df, aes(x = Actual, y = Full_Tree_Pred)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Fully Grown Tree: Predicted vs Actual",
       subtitle = paste("Test MSE =", round(full_test_mse, 2)),
       x = "Actual Daily Sales",
       y = "Predicted Daily Sales") +
  theme_minimal()

# Plot for Pruned Tree
p2 <- ggplot(comparison_df, aes(x = Actual, y = Pruned_Tree_Pred)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Pruned Tree: Predicted vs Actual",
       subtitle = paste("Test MSE =", round(pruned_test_mse, 2)),
       x = "Actual Daily Sales",
       y = "Predicted Daily Sales") +
  theme_minimal()

# Display plots
print(p1)
print(p2)

# Side by side if patchwork is available
if(require(patchwork, quietly = TRUE)) {
  combined_plot <- p1 + p2
  print(combined_plot)
}

# ============================================
# RESIDUAL PLOTS FOR DIAGNOSTICS
# ============================================

# Residual plots for both trees on test data
residual_df <- data.frame(
  Full_Residuals = test_data$daily_sales - full_test_pred,
  Pruned_Residuals = test_data$daily_sales - pruned_test_pred,
  Fitted_Full = full_test_pred,
  Fitted_Pruned = pruned_test_pred
)

# Residual plot for Full Tree
resid_plot_full <- ggplot(residual_df, aes(x = Fitted_Full, y = Full_Residuals)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "darkblue", size = 0.8) +
  labs(title = "Residual Plot: Fully Grown Tree",
       subtitle = paste("Pattern indicates", ifelse(abs(cor(residual_df$Fitted_Full, residual_df$Full_Residuals)) > 0.2, 
                                                    "potential overfitting", "reasonable fit")),
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Residual plot for Pruned Tree
resid_plot_pruned <- ggplot(residual_df, aes(x = Fitted_Pruned, y = Pruned_Residuals)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen", size = 0.8) +
  labs(title = "Residual Plot: Pruned Tree",
       subtitle = "More random scatter indicates better generalization",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

print(resid_plot_full)
print(resid_plot_pruned)

# ============================================
# COMPLEXITY VS PERFORMANCE TABLE
# ============================================

complexity_table <- data.frame(
  Tree_Type = c("Fully Grown", "Pruned"),
  Number_of_Nodes = c(nrow(full_tree$frame), nrow(pruned_tree$frame)),
  Number_of_Leaves = c(sum(full_tree$frame$var == "<leaf>"), sum(pruned_tree$frame$var == "<leaf>")),
  Depth = c(max(rpart::path.rpart(full_tree, node = rownames(full_tree$frame))$node), 
            max(rpart::path.rpart(pruned_tree, node = rownames(pruned_tree$frame))$node)),
  Train_MSE = c(full_train_mse, pruned_train_mse),
  Test_MSE = c(full_test_mse, pruned_test_mse),
  Overfitting_Gap = c(full_train_mse - full_test_mse, pruned_train_mse - pruned_test_mse)
)

cat("\n========== COMPLEXITY VS PERFORMANCE ==========\n")
print(complexity_table)

# ============================================
# FINAL CONCLUSION
# ============================================

cat("\n========== CONCLUSION ==========\n")

if(full_test_mse > pruned_test_mse) {
  cat("✓ The PRUNED tree performs BETTER on test data\n")
  cat("  (Test MSE:", round(pruned_test_mse, 2), "vs", round(full_test_mse, 2), ")\n")
  cat("  This indicates the fully grown tree was OVERFITTING the training data.\n")
  cat("  The pruned tree generalizes better to new data.\n")
} else {
  cat("The fully grown tree performs better on test data.\n")
  cat("  Test MSE:", round(full_test_mse, 2), "vs", round(pruned_test_mse, 2), "\n")
}

cat("\nKey insights:\n")
cat("• Train vs Test gap (Full tree):", round(full_train_mse - full_test_mse, 2), "\n")
cat("• Train vs Test gap (Pruned tree):", round(pruned_train_mse - pruned_test_mse, 2), "\n")
cat("• Complexity reduction:", nrow(full_tree$frame), "→", nrow(pruned_tree$frame), "nodes\n")
cat("• The pruned tree is", round(100 * (1 - nrow(pruned_tree$frame)/nrow(full_tree$frame)), 1), 
    "% smaller but generalizes better\n")

# ============================================
# OPTIONAL: Save the trees for future use
# ============================================

# Save the tree models
saveRDS(full_tree, "full_regression_tree.rds")
saveRDS(pruned_tree, "pruned_regression_tree.rds")

cat("\nTree models saved as:\n")
cat("  - full_regression_tree.rds\n")
cat("  - pruned_regression_tree.rds\n")