# Import libraries
library(readxl)
library(ggplot2)
library(reshape2)
library(knitr)
library(kableExtra)
library(gridExtra)
library(grid)
library(webshot2)
library(magick)
library(htmltools)
library(gt)
library(tidyr)
library(vtable)
library(dplyr)
library(lmtest)
library(car)

# Set up directory
setwd("C:/Users/willk/OneDrive/Desktop/AMS 572 Fall 2024 Group 01 Project")

# Read Excel file of dataset
data <- read_excel("C:/Users/willk/OneDrive/Desktop/AMS 572 Fall 2024 Group 01 Project/AMS572_Master_Excel.xlsx")

# Sort continuous and categorical variables
var_columns <- grep("^V[1-9][0-9]*", names(data), value = TRUE)
cat_columns <- grep("^V(16|17|18|19|20)", names(data), value = TRUE)
cont_columns <- setdiff(var_columns, cat_columns)

# Apply percent differencing transformation
transformed_data <- data
for (col in cont_columns) {
  if (is.numeric(data[[col]])) {transformed_data[[col]] <- c(NA, (data[[col]][-1] - data[[col]][-nrow(data)]) / data[[col]][-nrow(data)])}
}

# Convert categorical variables to factors
for (col in cat_columns) {
  transformed_data[[col]] <- as.factor(transformed_data[[col]])
}

# Print summary stats for continuous variables
summary_statistics_cont <- summary(transformed_data[, cont_columns])
print(summary_statistics_cont)

# Use st() to get better visualization of summary stats
numeric_data <- transformed_data[, cont_columns]
st(numeric_data, summ = list(c("notNA(x)", "mean(x)", "sd(x)", "min(x)", "pctile(x)[25]", "median(x)", "pctile(x)[75]", "max(x)")),
  summ.names=list(c("N", "Mean", "Std. Dev.", "Min", "Pctl. 25", "Median", "Pctl. 75", "Max")), file = "summary_table.png")

# Make frequency tables to assist in analyzing categorical variables
for (col in cat_columns) {
  cat("Frequency table for", col, ":\n")
  print(table(transformed_data[[col]]))
  cat("\n")
}

# Create one horizontal box plot for all continuous variables
# Combine all continuous variables into one frame
long_data <- transformed_data %>%
  select(all_of(cont_columns)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
actual_variable_names <- unique(long_data$Variable)

# Make sure the variables are in order
ordered_variable_names <- actual_variable_names[order(as.numeric(gsub("V", "", actual_variable_names)))]
long_data$Variable <- factor(long_data$Variable, levels = rev(ordered_variable_names))

# Plot the horizontal box plot containing all continuous variables
p <- ggplot(long_data, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  coord_flip() +                     
  ggtitle("Boxplot of Transformed Continuous Variables") +
  xlab("Variable") +
  ylab("Monthly Percent Change") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.25))
ggsave(filename = "combined_boxplot.png", plot = p, width = 8, height = 6)

# Create bar plots for all of the categorical variables
# Combine all ategorical variables into one frame
facet_data <- transformed_data %>%
  pivot_longer(cols = all_of(cat_columns), names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(Value))

# Plot the bar plots
facet_plot <- ggplot(facet_data, aes(x = Value)) +
  geom_bar(fill = "blue", color = "black") +
  facet_wrap(~ Variable, scales = "free", ncol = 2) +
  labs(y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12)
  ) +
  ggtitle("Bar Plots of Categorical Variables")
ggsave("facet_barplots.png", facet_plot, width = 10, height = 8)

# SPX percent return
data$SPX_return <- c(NA, (data$SPX[-1] - data$SPX[-nrow(data)]) / data$SPX[-nrow(data)])

# RUT percent return
data$RUT_return <- c(NA, (data$RUT[-1] - data$RUT[-nrow(data)]) / data$RUT[-nrow(data)])

# Spread Return (RUT - SPX)
data$Spread_return <- data$RUT_return - data$SPX_return

# Make scatter plots to compare each continuous variable with the spread return
# Divide the continuous variables into groups of 4 for easier plotting in the document
cont_columns_groups <- split(cont_columns, ceiling(seq_along(cont_columns) / 4))

# Loop through each group of 4 and make scatter plots
for (i in seq_along(cont_columns_groups)) {
  group_vars <- cont_columns_groups[[i]]
  group_data <- transformed_data %>%
    select(all_of(group_vars)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Percent_Change") %>%
    mutate(
      Spread_Return = rep(data$Spread_return, length(group_vars)),
      Variable = factor(Variable, levels = group_vars)
    ) %>%
    filter(!is.na(Spread_Return), !is.na(Percent_Change))
  
  scatter_plot <- ggplot(group_data, aes(x = Percent_Change, y = Spread_Return)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    facet_wrap(~ Variable, scales = "free", ncol = 2) +
    labs(x = "Percent Change", y = "Spread Return (RUT - SPX)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      strip.text = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    ) +
    ggtitle(paste("Scatter Plots of Percent Changes vs Spread Return ( Group", i, ")"))
  
  ggsave(
    filename = paste0("scatterplots_group_", i, ".png"),
    plot = scatter_plot,
    width = 12,
    height = 8
  )
}

# Get the correlation matrix of continuous variables
continuous_data <- transformed_data[, cont_columns]
correlation_matrix <- cor(continuous_data, use = "complete.obs")

# Make a heatmap of the correlations, including labels for each variable and correlation value inside each square
melted_corr <- melt(correlation_matrix)
melted_corr$Var2 <- factor(melted_corr$Var2, levels = rev(unique(melted_corr$Var2)))
heatmap_plot <- ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Create the heatmap tiles
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 3) +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", midpoint = 0, 
    limit = c(-1, 1), space = "Lab", name = "Correlation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  ggtitle("Correlation Matrix Heatmap")
ggsave(filename = "correlation_heatmap_with_values.png", plot = heatmap_plot, width = 10, height = 8)

# Start doing multiple regression
regression_data <- transformed_data %>%
  select(all_of(c(cont_columns, cat_columns))) %>%
  mutate(Spread_Return = data$Spread_return) %>%
  filter(!is.na(Spread_Return))

# Make sure categorical variables are factors
regression_data[cat_columns] <- lapply(regression_data[cat_columns], as.factor)

# Create a second-order model (i.e., containing both linear and quadratic terms)
quadratic_terms <- paste0("I(`", cont_columns, "`^2)", collapse = " + ")
combined_formula <- as.formula(paste("Spread_Return ~ . +", quadratic_terms))

# Fit the multiple regression model
combined_model <- lm(combined_formula, data = regression_data)
summary(combined_model)
sink("combined_model_summary.txt")
print(summary(combined_model))
sink()

# Now make a regular multiple linear regression model (no quadratic or higher-order terms)
regression_data <- transformed_data %>%
  select(all_of(c(cont_columns, cat_columns))) %>%
  mutate(Spread_Return = data$Spread_return) %>%
  filter(!is.na(Spread_Return))

# Ensure categorical variables are factors (already done earlier, but confirm)
#regression_data[cat_columns] <- lapply(regression_data[cat_columns], as.factor)

# Fit the multiple linear regression model
linear_model <- lm(Spread_Return ~ ., data = regression_data)
summary(linear_model)
sink("linear_model_summary.txt")
print(summary(linear_model))
sink()

# Calculate VIF values
vif_values <- vif(linear_model)
print(vif_values)

# Input which variables to exclude based on VIF results
variables_to_exclude <- c("V15_(US_10y_yield)")
regression_data_reduced <- regression_data %>%
  select(-all_of(variables_to_exclude))

# Fit another multiple linear regression model without high VIF values
linear_model_reduced <- lm(Spread_Return ~ ., data = regression_data_reduced)
summary(linear_model_reduced)
sink("linear_model_reduced_summary.txt")
print(summary(linear_model_reduced))
sink()

# Calculate new VIF values
vif_values2 <- vif(linear_model_reduced)
print(vif_values2)

# Input which variables to exclude based on VIF results
variables_to_exclude <- c("V15_(US_10y_yield)","V3_(High_yield_yields)")
regression_data_reduced <- regression_data %>%
  select(-all_of(variables_to_exclude))

# Fit another multiple linear regression model without high VIF values
linear_model_final <- lm(Spread_Return ~ ., data = regression_data_reduced)
summary(linear_model_final)
sink("linear_model_final.txt")
print(summary(linear_model_final))
sink()

# Check final VIF values
vif_values_final <- vif(linear_model_final)
print(vif_values_final)

# Get residuals
residuals <- resid(linear_model_final)

# Get fitted values
fitted_values <- fitted(linear_model_final)

# Make a residual plot
residual_plot <- ggplot(data = NULL, aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = "Residual Plot"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("residual_plot.png", residual_plot, width = 8, height = 6)

# Make a QQ Plot with residuals adjusted by mean and sd
adjusted_residuals <- (residuals - mean(residuals)) / sd(residuals)
qq_plot <- ggplot(data = NULL, aes(sample = adjusted_residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Standardized Residuals", x = "Theoretical Quantiles", y = "Standardized Sample Quantiles") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
print(qq_plot)

# Kolmogorov–Smirnov test for normality
ks_test <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
cat("Kolmogorov–Smirnov Test for Normality:\n")
print(ks_test)

# Breusch-Pagan test for homoscedasticity
bp_test <- bptest(linear_model_final)
cat("\nBreusch-Pagan Test for Homoscedasticity:\n")
print(bp_test)

# Simulate MCAR for 20% of values in 3 given variables
set.seed(123)
variables_to_miss <- c("V1_(CCC_and_lower_yield)", "V4_(Copper)", "V7_(Gold)")
regression_data_with_mcar <- regression_data
missing_percentage <- 0.2
for (var in variables_to_miss) {
  missing_indices <- sample(1:nrow(regression_data_with_mcar), 
                            size = round(nrow(regression_data_with_mcar) * missing_percentage), 
                            replace = FALSE)
  regression_data_with_mcar[missing_indices, var] <- NA
}

# For any NA values, replace it with the rolling median of previous values
for (var in variables_to_miss) {
  regression_data_with_mcar[[var]] <- zoo::na.aggregate(
    regression_data_with_mcar[[var]],
    FUN = function(x) {
      rollapplyr(
        x,
        width = seq_along(x),
        FUN = median,
        partial = TRUE,
        na.rm = TRUE
      )
    }
  )
}

# Exclude these variables based on VIF
variables_to_exclude_mcar <- c("V15_(US_10y_yield)")
regression_data_reduced_mcar <- regression_data_with_mcar %>%
  select(-all_of(variables_to_exclude_mcar))

# Fit again the MCAR multiple linear regression model after variables with high VIFs were removed
linear_model_mcar <- lm(Spread_Return ~ ., data = regression_data_reduced_mcar)

# Check MCAR VIF values
vif_values_mcar <- vif(linear_model_mcar)
print(vif_values_mcar)

# Get the summary
sink("linear_model_after_mcar.txt")
print(summary(linear_model_mcar))
sink()

# Get residuals for MCAR 
residuals <- resid(linear_model_mcar)

# Get fitted values for MCAR
fitted_values <- fitted(linear_model_mcar)

# MCAR Kolmogorov–Smirnov test for normality
ks_test <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
cat("Kolmogorov–Smirnov Test for Normality:\n")
print(ks_test)

# MCAR Breusch-Pagan test for homoscedasticity
bp_test <- bptest(linear_model_mcar)
cat("\nBreusch-Pagan Test for Homoscedasticity:\n")
print(bp_test)

# Simulate MNAR for 20% of values in 3 given variables
set.seed(321)  # Set different seed
variables_to_miss <- c("V1_(CCC_and_lower_yield)", "V4_(Copper)", "V7_(Gold)")
regression_data_with_mnar <- regression_data
missing_percentage <- 0.1
for (var in variables_to_miss) {
  missing_indices <- sample(1:nrow(regression_data_with_mnar), 
                            size = round(nrow(regression_data_with_mnar) * missing_percentage), 
                            replace = FALSE)
  regression_data_with_mnar[missing_indices, var] <- NA
}

# For any NA values, replace it with the rolling 5th percentile of previous values
for (var in variables_to_miss) {
  regression_data_with_mnar[[var]] <- zoo::na.aggregate(
    regression_data_with_mnar[[var]], 
    FUN = function(x) rollapplyr(
      x, 
      width = seq_along(x),
      FUN = function(y) quantile(y, probs = 0.05, na.rm = TRUE), 
      fill = NA,
      partial = TRUE
    )
  )
}

# Exclude these variables based on VIF
variables_to_exclude_mnar <- c("V15_(US_10y_yield)")
regression_data_reduced_mnar <- regression_data_with_mnar %>%
  select(-all_of(variables_to_exclude_mnar))

# Fit again the MNAR multiple linear regression model after variables with high VIFs were removed
linear_model_mnar <- lm(Spread_Return ~ ., data = regression_data_reduced_mnar)

# Check MNAR VIF values
vif_values_mnar <- vif(linear_model_mnar)
print(vif_values_mnar)

# Get the summary
sink("linear_model_after_mnar.txt")
print(summary(linear_model_mnar))
sink()

# Get residuals
residuals <- resid(linear_model_mnar)

# Get fitted values
fitted_values <- fitted(linear_model_mnar)

# MNAR Kolmogorov–Smirnov test for normality
ks_test <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
cat("Kolmogorov–Smirnov Test for Normality:\n")
print(ks_test)

# MNAR Breusch-Pagan test for homoscedasticity
bp_test <- bptest(linear_model_mnar)
cat("\nBreusch-Pagan Test for Homoscedasticity:\n")
print(bp_test)