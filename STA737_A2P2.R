# Load required libraries
library(rpart)
library(rpart.plot)
library(dplyr)

# Read the data

setwd("C:\\Users\\Administrator\\Desktop\\STA 737")
df <- read.csv("Grocery_Cleaned.csv")

# View the data structure
head(df)

# ============================================
# FIT A REGRESSION TREE
# ============================================

# We'll predict daily_sales using footfall and stock_level
tree_model <- rpart(daily_sales ~ footfall + stock_level, 
                    data = df,
                    method = "anova",  # for regression
                    control = rpart.control(minsplit = 2,  # minimum observations in node to split
                                            cp = 0,        # complexity parameter (0 = allow full tree)
                                            maxdepth = 1)) # limit to first split only

# Print the tree details
print(tree_model)

# Visualize the tree
rpart.plot(tree_model, main = "Regression Tree - First Split", 
           type = 2, extra = 101, under = TRUE, 
           box.palette = "RdBu", branch = 1)

# ============================================
# EXTRACT THE FIRST SPLIT INFORMATION
# ============================================

# Get the split information
split_info <- tree_model$splits
print(split_info)

# Get the variable used for split
split_variable <- rownames(split_info)[1]
split_value <- split_info[1, "index"]

cat("\n========== FIRST SPLIT RULE ==========\n")
cat("Split variable:", split_variable, "\n")
cat("Split value:", round(split_value, 2), "\n")
cat("Rule:", split_variable, "<", round(split_value, 2), "\n\n")

# ============================================
# CALCULATE PREDICTED VALUES FOR EACH TERMINAL NODE
# ============================================

# Get the node assignments for each observation
node_indices <- tree_model$where

# Get the predicted values (fitted values from the tree)
predicted_values <- predict(tree_model)

# Calculate mean predicted value for each node
node1_pred <- mean(predicted_values[node_indices == 1])
node2_pred <- mean(predicted_values[node_indices == 2])

cat("========== PREDICTED VALUES ==========\n")
cat("Left node (", split_variable, "<", round(split_value, 2), "):", 
    round(node1_pred, 2), "\n")
cat("Right node (", split_variable, ">=", round(split_value, 2), "):", 
    round(node2_pred, 2), "\n\n")

# ============================================
# MANUAL CALCULATION OF THE FIRST SPLIT
# ============================================

# Method 1: Manual calculation using the actual split
cat("========== MANUAL CALCULATIONS ==========\n")

# Identify which observations go to left and right nodes based on the split
if(split_variable == "footfall") {
  left_node <- df[df$footfall < split_value, ]
  right_node <- df[df$footfall >= split_value, ]
} else if(split_variable == "stock_level") {
  left_node <- df[df$stock_level < split_value, ]
  right_node <- df[df$stock_level >= split_value, ]
}

# Calculate predicted values (mean of daily_sales in each node)
left_node_pred <- mean(left_node$daily_sales)
right_node_pred <- mean(right_node$daily_sales)

cat("Based on the split", split_variable, "<", round(split_value, 2), ":\n")
cat("Left node observations:", nrow(left_node), "\n")
cat("Left node predicted value (mean):", round(left_node_pred, 2), "\n\n")
cat("Right node observations:", nrow(right_node), "\n")
cat("Right node predicted value (mean):", round(right_node_pred, 2), "\n\n")

# ============================================
# CALCULATE RESIDUAL SUM OF SQUARES FOR THE RIGHT NODE
# ============================================

# Calculate residuals for the right node
right_node_residuals <- right_node$daily_sales - right_node_pred

# Calculate Residual Sum of Squares (RSS)
rss_right <- sum(right_node_residuals^2)

# Calculate Mean Squared Error for the right node
mse_right <- mean(right_node_residuals^2)

# Calculate Total Sum of Squares for the right node (variation before splitting)
tss_right <- sum((right_node$daily_sales - mean(right_node$daily_sales))^2)

cat("========== RIGHT NODE CALCULATIONS ==========\n")
cat("Right node (", split_variable, ">=", round(split_value, 2), ")\n")
cat("Number of observations in right node:", nrow(right_node), "\n")
cat("Predicted value for right node:", round(right_node_pred, 2), "\n\n")

cat("Residuals (actual - predicted):\n")
print(round(head(right_node_residuals, 10), 2))
cat("... (showing first 10 of", nrow(right_node), "observations)\n\n")

cat("Residual Sum of Squares (RSS) for right node:\n")
cat("RSS = Σ(actual - predicted)²\n")
cat("RSS =", round(rss_right, 2), "\n\n")

cat("Mean Squared Error (MSE) for right node:\n")
cat("MSE = RSS / n =", round(rss_right, 2), "/", nrow(right_node), 
    "=", round(mse_right, 2), "\n\n")

# ============================================
# DETAILED STEP-BY-STEP RSS CALCULATION
# ============================================

cat("========== DETAILED RSS CALCULATION STEPS ==========\n")
cat("Step 1: Calculate predicted value for right node\n")
cat("        ŷ_right = mean(y_right) =", round(right_node_pred, 2), "\n\n")

cat("Step 2: Calculate residuals for each observation\n")
cat("        e_i = y_i - ŷ_right\n\n")

# Create a table showing calculations for first few observations
calc_table <- data.frame(
  Obs = 1:min(10, nrow(right_node)),
  Actual = round(head(right_node$daily_sales, 10), 2),
  Predicted = rep(round(right_node_pred, 2), 10),
  Residual = round(head(right_node_residuals, 10), 2),
  Squared_Residual = round(head(right_node_residuals^2, 10), 2)
)

print(calc_table)
cat("...\n\n")

cat("Step 3: Square each residual\n")
cat("        e_i² = (y_i - ŷ_right)²\n\n")

cat("Step 4: Sum all squared residuals\n")
cat("        RSS = Σ e_i²\n")
cat("        RSS =", round(rss_right, 2), "\n\n")

# Show the summation
cat("Alternative representation:\n")
cat("RSS = Σ(y_i - ŷ_right)² for i in right node\n")
cat("RSS = Σ(y_i -", round(right_node_pred, 2), ")²\n")
cat("RSS =", paste(round(head(right_node_residuals^2, 5), 2), collapse = " + "), 
    if(nrow(right_node) > 5) paste(" + ... +", round(tail(right_node_residuals^2, 1), 2)), "\n")
cat("RSS =", round(rss_right, 2), "\n\n")

# ============================================
# COMPARE WITH LEFT NODE
# ============================================

left_node_residuals <- left_node$daily_sales - left_node_pred
rss_left <- sum(left_node_residuals^2)

total_rss <- rss_left + rss_right
total_tss <- sum((df$daily_sales - mean(df$daily_sales))^2)
r_squared <- 1 - (total_rss / total_tss)

cat("========== COMPLETE TREE SUMMARY ==========\n")
cat("Total observations:", nrow(df), "\n")
cat("Left node RSS:", round(rss_left, 2), "\n")
cat("Right node RSS:", round(rss_right, 2), "\n")
cat("Total RSS (both nodes):", round(total_rss, 2), "\n")
cat("Total Sum of Squares (TSS):", round(total_tss, 2), "\n")
cat("R-squared for this split:", round(r_squared, 4), "\n\n")

# ============================================
# VISUALIZE THE RESIDUALS FOR RIGHT NODE
# ============================================

# Create residual plot for right node
right_node$residuals <- right_node_residuals
right_node$index <- 1:nrow(right_node)

residual_plot <- ggplot(right_node, aes(x = index, y = residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = paste("Residuals for Right Node (", split_variable, ">=", round(split_value, 2), ")"),
       subtitle = paste("RSS =", round(rss_right, 2), "| MSE =", round(mse_right, 2)),
       x = "Observation Index",
       y = "Residual (Actual - Predicted)") +
  theme_minimal()

print(residual_plot)

# ============================================
# HISTOGRAM OF RESIDUALS FOR RIGHT NODE
# ============================================

histogram_plot <- ggplot(right_node, aes(x = residuals)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Residuals in Right Node",
       subtitle = paste("Mean residual:", round(mean(right_node_residuals), 2), 
                        "| SD:", round(sd(right_node_residuals), 2)),
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

print(histogram_plot)

# ============================================
# OUTPUT THE FINAL ANSWER FORMAT
# ============================================

cat("\n========== FINAL ANSWER ==========\n")
cat("Regression Tree - First Split:\n")
cat("• Predictor variable:", split_variable, "\n")
cat("• Split value:", round(split_value, 2), "\n")
cat("• Rule:", split_variable, "<", round(split_value, 2), "\n\n")

cat("Predicted values:\n")
cat("• Left node (", split_variable, "<", round(split_value, 2), "):", 
    round(left_node_pred, 2), "\n")
cat("• Right node (", split_variable, ">=", round(split_value, 2), "):", 
    round(right_node_pred, 2), "\n\n")

cat("Residual Sum of Squares for Right Node:\n")
cat("• Number of observations (n):", nrow(right_node), "\n")
cat("• Predicted value (ȳ):", round(right_node_pred, 2), "\n")
cat("• RSS = Σ(y_i - ȳ)² =", round(rss_right, 2), "\n\n")

cat("Step-by-step:\n")
cat("RSS =", paste(round(head(right_node_residuals^2, 3), 2), collapse = " + "), 
    if(nrow(right_node) > 3) paste(" + ... +", round(tail(right_node_residuals^2, 1), 2)), "\n")
cat("RSS =", round(rss_right, 2), "\n")

#----------------------------------------------
# Classification Tree
#----------------------------------------------
# Load required libraries
library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)

# Read the data
df <- read.csv("Grocery_Cleaned.csv")

# View the inventory_status distribution
cat("========== TARGET VARIABLE: INVENTORY STATUS ==========\n")
cat("Class distribution:\n")
print(table(df$inventory_status))
cat("\n")
cat("Proportions:\n")
print(prop.table(table(df$inventory_status)))
cat("\n")

# ============================================
# FIT CLASSIFICATION TREE
# ============================================

# Fit tree using all relevant predictors
# Convert inventory_status to factor if not already
df$inventory_status <- as.factor(df$inventory_status)
# Convert categorical predictors
df$store_format <- as.factor(df$store_format)
df$promotion_active <- as.factor(df$promotion_active)

class_tree <- rpart(inventory_status ~ daily_sales + footfall + stock_level + 
                      unit_price + store_format + promotion_active,
                    data = df,
                    method = "class",
                    control = rpart.control(minsplit = 20,    # Minimum observations to split
                                            cp = 0.01,        # Complexity parameter
                                            maxdepth = 3))    # Limit depth for interpretability

# Print the tree details
cat("\n========== CLASSIFICATION TREE SUMMARY ==========\n")
print(class_tree)

# Visualize the tree
rpart.plot(class_tree, main = "Classification Tree: Predicting Inventory Status",
           type = 2, extra = 104, under = TRUE, 
           box.palette = "RdBu", branch = 1, 
           nn = TRUE, fallen.leaves = TRUE)

# ============================================
# DISPLAY NODE INFORMATION
# ============================================
# Get node information - CORRECTED COLUMN NAMES
node_info <- class_tree$frame
node_info$node_number <- as.numeric(rownames(node_info))

# Check column names to see what's available
cat("\nColumn names in node_info:\n")
print(names(node_info))
      
# Get node information
node_info <- class_tree$frame
node_info$node_number <- as.numeric(rownames(node_info))
node_info <- node_info[, c("node_number", "var", "n", "dev", "yval", "yval2")]

cat("\n========== NODE INFORMATION ==========\n")
print(node_info)

# ============================================
# STEP 1: CHOOSE A TERMINAL NODE AND CALCULATE GINI INDEX
# ============================================

# Identify terminal nodes (where var == "<leaf>")
terminal_nodes <- node_info[node_info$var == "<leaf>", ]

cat("\n========== TERMINAL NODES ==========\n")
print(terminal_nodes)

# Select the first terminal node (or choose a specific one)
selected_node <- terminal_nodes[1, ]
node_number <- selected_node$node_number
node_n <- selected_node$n

# Get the node assignments for each observation
node_assignments <- class_tree$where

# For the selected node, get actual class proportions
selected_node_obs <- df[node_assignments == node_number, ]
node_class_props <- prop.table(table(selected_node_obs$inventory_status))

# Extract probabilities (adjust class names as they appear)
class_names <- names(node_class_props)
p_high <- ifelse("High_Priority" %in% class_names, 
                 node_class_props["High_Priority"], 0)
p_low <- ifelse("Low_Priority" %in% class_names, 
                node_class_props["Low_Priority"], 0)

# Calculate Gini index for the selected terminal node
gini_terminal <- 1 - sum(node_class_props^2)

# Extract tree frame
node_info <- class_tree$frame

# Terminal nodes
terminal_nodes <- node_info[grepl("leaf", node_info$var), ]

cat("\n========== TERMINAL NODES ==========\n")
print(terminal_nodes)

# Select first terminal node
selected_node <- terminal_nodes[1, , drop = FALSE]

# IMPORTANT:
# Use rownames as node IDs
node_number <- as.numeric(rownames(selected_node))

cat("\nSelected node:", node_number, "\n")

# Node assignments from tree
node_assignments <- class_tree$where

# Observations in selected node
selected_node_obs <- df[node_assignments == node_number, , drop = FALSE]

cat("Observations in node:", nrow(selected_node_obs), "\n")

# Class proportions
node_class_props <- prop.table(table(selected_node_obs$inventory_status))

print(node_class_props)

# Gini index
gini_terminal <- 1 - sum(node_class_props^2)

cat("\nGini Index =", round(gini_terminal, 4), "\n")

# Interpretation in context
cat("Interpretation in context of inventory status:\n")
if(gini_terminal == 0) {
  cat("  ✓ This node is PURE - all products have the SAME inventory priority.\n")
  cat("  ✓ Every observation in this node is either ALL High_Priority or ALL Low_Priority.\n")
  cat("  ✓ No uncertainty in predicting inventory status for this group.\n")
} else if(gini_terminal < 0.2) {
  cat("  ✓ This node has HIGH PURITY (low impurity).\n")
  cat("  ✓ Most products in this node share the same inventory priority.\n")
  cat("  ✓ Example: If p_high = 0.9, then 90% of products need high priority restocking.\n")
  cat("  ✓ The split leading to this node successfully separated inventory priorities.\n")
} else if(gini_terminal < 0.4) {
  cat("  ◯ This node has MODERATE IMPURITY.\n")
  cat("  ◯ There is a meaningful mix of High_Priority and Low_Priority products.\n")
  cat("  ◯ Example: 70% Low_Priority, 30% High_Priority indicates some uncertainty.\n")
  cat("  ◯ Further splitting could better distinguish inventory priorities.\n")
} else {
  cat("  ✗ This node has HIGH IMPURITY (Gini > 0.4).\n")
  cat("  ✗ Products are almost evenly split between High and Low priority.\n")
  cat("  ✗ Example: 55% Low_Priority, 45% High_Priority - very uncertain.\n")
  cat("  ✗ This node should likely be split further for better classification.\n")
}

# ============================================
# STEP 2: CALCULATE WEIGHTED GINI FOR THREE PREDICTORS
# ============================================

# Extract split information from the tree
split_info <- class_tree$splits

cat("\n========== SPLIT INFORMATION FROM TREE ==========\n")
print(split_info)

# Identify predictors that appear in the tree
predictors_in_tree <- unique(rownames(class_tree$splits))

# Remove surrogate splits and NA values
predictors_in_tree <- predictors_in_tree[
  !is.na(predictors_in_tree) &
    predictors_in_tree != "<leaf>"
]

cat("\nPredictors used in tree:", paste(predictors_in_tree, collapse = ", "), "\n")

# Take the top 3 predictors (or all if less than 3)
top_3_predictors <- predictors_in_tree[1:min(3, length(predictors_in_tree))]

# Calculate root node Gini (before any split)
root_props <- prop.table(table(df$inventory_status))
p_high_root <- ifelse("High_Priority" %in% names(root_props), 
                      root_props["High_Priority"], 0)
p_low_root <- ifelse("Low_Priority" %in% names(root_props), 
                     root_props["Low_Priority"], 0)
root_gini <- 1 - (p_high_root^2 + p_low_root^2)

cat("\n========== WEIGHTED GINI FOR TOP 3 PREDICTORS ==========\n")
cat("Root node Gini (overall impurity):", round(root_gini, 4), "\n\n")

# Store results
weighted_gini_results <- data.frame(
  Predictor = character(),
  Split_Value = numeric(),
  Weighted_Gini = numeric(),
  Gini_Reduction = numeric(),
  stringsAsFactors = FALSE
)

# Function to calculate Gini for a subset
calc_gini <- function(subset) {
  if(nrow(subset) == 0) return(0)
  
  props <- prop.table(table(subset$inventory_status))
  
  # General Gini formula for any number of classes
  gini <- 1 - sum(props^2)
  
  return(gini)
}

for(pred in top_3_predictors) {
  # Get split info for this predictor
  pred_splits <- split_info[rownames(split_info) == pred, , drop = FALSE]
  
  if(length(pred_splits) > 0 && nrow(as.data.frame(pred_splits)) > 0) {
    split_val <- pred_splits[1, "index"]
    
    # For categorical predictors like store_format
    if(pred == "store_format") {
      # Get unique categories
      categories <- unique(df[[pred]])
      total_n <- nrow(df)
      weighted_gini <- 0
      
      for(cat in categories) {
        subset_data <- df[df[[pred]] == cat, ]
        n_cat <- nrow(subset_data)
        if(n_cat > 0) {
          gini_cat <- calc_gini(subset_data)
          weighted_gini <- weighted_gini + (n_cat / total_n) * gini_cat
        }
      }
      gini_reduction <- root_gini - weighted_gini
      
      weighted_gini_results <- rbind(weighted_gini_results,
                                     data.frame(Predictor = pred,
                                                Split_Value = NA,
                                                Weighted_Gini = round(weighted_gini, 4),
                                                Gini_Reduction = round(gini_reduction, 4),
                                                stringsAsFactors = FALSE))
    } 
    # For continuous predictors
    else {
      # Split based on the value from the tree
      left_data <- df[df[[pred]] < split_val, ]
      right_data <- df[df[[pred]] >= split_val, ]
      
      gini_left <- calc_gini(left_data)
      gini_right <- calc_gini(right_data)
      
      total_n <- nrow(df)
      weighted_gini <- (nrow(left_data)/total_n * gini_left) + 
        (nrow(right_data)/total_n * gini_right)
      
      gini_reduction <- root_gini - weighted_gini
      
      weighted_gini_results <- rbind(weighted_gini_results,
                                     data.frame(Predictor = pred,
                                                Split_Value = round(split_val, 2),
                                                Weighted_Gini = round(weighted_gini, 4),
                                                Gini_Reduction = round(gini_reduction, 4),
                                                stringsAsFactors = FALSE))
    }
  }
}

print(weighted_gini_results)

# ============================================
# WHICH PREDICTOR PRODUCES THE BEST SPLIT?
# ============================================

cat("\n========== BEST SPLIT ANALYSIS ==========\n")

# Find the predictor with lowest weighted Gini (best split)
best_by_gini <- weighted_gini_results[which.min(weighted_gini_results$Weighted_Gini), ]
best_by_reduction <- weighted_gini_results[which.max(weighted_gini_results$Gini_Reduction), ]

cat("Predictor with LOWEST Weighted Gini (purer child nodes):\n")
cat("  →", best_by_gini$Predictor, 
    "(Weighted Gini =", best_by_gini$Weighted_Gini, 
    "| Reduction =", best_by_gini$Gini_Reduction, ")\n\n")

cat("Predictor with GREATEST Gini Reduction (most impurity eliminated):\n")
cat("  →", best_by_reduction$Predictor, 
    "(Reduction =", best_by_reduction$Gini_Reduction, 
    "| Weighted Gini =", best_by_reduction$Weighted_Gini, ")\n\n")

# Determine the best split
best_predictor <- best_by_gini$Predictor

cat("✓ BEST SPLIT: The predictor '", best_predictor, 
    "' produces the best split for predicting inventory status.\n", sep = "")
cat("\nJustification:\n")
cat("  • Weighted Gini =", best_by_gini$Weighted_Gini, "(lowest among all predictors)\n")
cat("  • Gini Reduction =", best_by_gini$Gini_Reduction, "(from", round(root_gini, 4), ")\n")
cat("  • Split value =", ifelse(is.na(best_by_gini$Split_Value), "Categorical", best_by_gini$Split_Value), "\n\n")

cat("Explanation in context of inventory management:\n")
cat("• The best split creates child nodes that are most pure in terms of\n")
cat("  inventory priority (High_Priority vs Low_Priority).\n")
cat("• This means the predictor", best_predictor, "is the strongest indicator\n")
cat("  of whether a product needs high-priority restocking.\n")
cat("• The weighted Gini of", best_by_gini$Weighted_Gini, "indicates that after\n")
cat("  splitting on", best_predictor, ", the remaining impurity is relatively low.\n")

# ============================================
# VISUALIZE THE BEST SPLIT
# ============================================

# Create visualization for the best split (if continuous)
if(!is.na(best_by_gini$Split_Value)) {
  p <- ggplot(df, aes(x = .data[[best_predictor]], 
                      y = as.numeric(inventory_status == "High_Priority"), 
                      color = inventory_status)) +
    geom_jitter(alpha = 0.5, width = 0, height = 0.02, size = 1.5) +
    geom_vline(xintercept = best_by_gini$Split_Value, 
               color = "red", linetype = "dashed", linewidth = 1.2) +
    labs(title = paste("Best Split for Predicting Inventory Status:", best_predictor),
         subtitle = paste("Split value =", best_by_gini$Split_Value,
                          "| Weighted Gini =", best_by_gini$Weighted_Gini),
         x = best_predictor,
         y = "High Priority (1) vs Low Priority (0)") +
    theme_minimal() +
    annotate("text", x = best_by_gini$Split_Value + max(df[[best_predictor]], na.rm = TRUE) * 0.05, 
             y = 0.9, size = 3.5, hjust = 0,
             label = paste("Right node\n(>=", best_by_gini$Split_Value, ")")) +
    annotate("text", x = best_by_gini$Split_Value - max(df[[best_predictor]], na.rm = TRUE) * 0.05, 
             y = 0.9, size = 3.5, hjust = 1,
             label = paste("Left node\n(<", best_by_gini$Split_Value, ")"))
  
  print(p)
}

# ============================================
# DETAILED GINI CALCULATION FOR EACH PREDICTOR
# ============================================

cat("\n========== DETAILED GINI CALCULATIONS ==========\n")

for(i in 1:nrow(weighted_gini_results)) {
  pred <- weighted_gini_results[i, "Predictor"]
  w_gini <- weighted_gini_results[i, "Weighted_Gini"]
  reduction <- weighted_gini_results[i, "Gini_Reduction"]
  
  cat("\n", i, ". Predictor:", pred, "\n")
  cat("   Weighted Gini =", w_gini, "\n")
  cat("   Gini Reduction =", reduction, "\n")
  
  if(pred == best_predictor) {
    cat("   ✓ BEST SPLIT - This predictor creates the purest child nodes\n")
    cat("     for classifying inventory status.\n")
  }
}

# ============================================
# SUMMARY TABLE
# ============================================

cat("\n\n========== FINAL SUMMARY ==========\n")

# Create a summary data frame
summary_df <- data.frame(
  Metric = c("Target Variable", "Root Node Gini", "Best Predictor", 
             "Best Split Weighted Gini", "Gini Reduction", "Terminal Node Gini"),
  Value = c("Inventory Status (High_Priority vs Low_Priority)", 
            round(root_gini, 4),
            best_predictor,
            round(best_by_gini$Weighted_Gini, 4),
            round(best_by_gini$Gini_Reduction, 4),
            round(gini_terminal, 4))
)

print(summary_df)

cat("\nInterpretation of the Gini Index in Inventory Context:\n")
cat("• The Gini index measures how 'mixed' a node is between High and Low priority products.\n")
cat("• Gini = 0: All products have the SAME inventory priority - perfect for decision making.\n")
cat("• Gini = 0.5: Equal mix of High and Low priority - maximum uncertainty.\n")
cat("• Lower Gini after splitting indicates we've successfully distinguished\n")
cat("  which products need urgent restocking versus those that can wait.\n")
