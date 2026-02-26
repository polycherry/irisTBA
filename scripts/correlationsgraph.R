################################################################################
# Correlations Analysis
# Purpose: Calculate and visualize correlations between features and PC scores
################################################################################

library(ggplot2)
library(dplyr)
library(tidyr)

################################################################################
# 1. Correlation of variables with PC_combined
################################################################################

# Calculate correlations for all numeric variables vs PC_combined
cor_combined <- feature_df %>%
  select(where(is.numeric)) %>%
  summarise(across(-PC_combined, 
                   ~ cor(.x, feature_df$PC_combined, use = "complete.obs"))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Correlation")

# barplot of correlations
ggplot(cor_combined, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(title = "Correlation of Variables with PC_combined",
       x = "Variable",
       y = "Correlation")

################################################################################
# 2. Inspect actual values in outliers
################################################################################

# Subset outlier data
outlier_data <- feature_df %>%
  filter(outlier == TRUE)

# Convert to long format for plotting
outlier_long <- outlier_data %>%
  pivot_longer(cols = -c(PC_combined, outlier), 
               names_to = "Variable", values_to = "Value")

# Plot values of outliers vs non-outliers for each variable
ggplot(outlier_long, aes(x = Variable, y = Value, color = outlier)) +
  geom_jitter(width = 0.2, alpha = 0.7) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(title = "Variable Values in Outliers vs Non-Outliers",
       x = "Variable",
       y = "Value")
