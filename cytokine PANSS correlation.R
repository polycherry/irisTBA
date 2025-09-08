############################################################
# PCA correlation pipeline
# Author: Vasishta
# Date: Aug 2025
# Purpose: 
#   - Take PCA results (pca_df)
#   - Clean up the ID column (remove 'a')
#   - Match with the large dataset (dataset_IRIS_for_Poly_22_08_2025)
#   - Compute correlations between PCA axes and all variables
#   - Output a correlation table and visualize results
############################################################

# Load required packages
library(dplyr)
library(ggplot2)
library(reshape2)

############################################################
# Step 1. Starting point: PCA dataframe (already created)
# pca_df should contain PC1, PC2, Image (or Sample ID), etc.
# Example structure assumed:
# pca_df <- data.frame(
#   PC1 = ...,
#   PC2 = ...,
#   Image = ...,
#   SubjectID = ...,
#   Group = ...
# )
############################################################


# Step 2. Clean up the SubjectID (remove trailing "a")
pca_df <- pca_df %>%
  mutate(SubjectID_clean = gsub("a$", "", SubjectID))

# Remove leading/trailing whitespace and convert to numeric
dataset_IRIS_for_Poly_22_08_2025$ID_clean <- as.numeric(trimws(dataset_IRIS_for_Poly_22_08_2025$ID))

# Also make sure pca_df has numeric IDs
pca_df$SubjectID_clean <- as.numeric(pca_df$SubjectID_clean)

# Now join safely on the numeric IDs
merged_df <- inner_join(pca_df, dataset_IRIS_for_Poly_22_08_2025, 
                        by = c("SubjectID_clean" = "ID_clean"))

# Check how many rows matched
nrow(merged_df)


############################################################
# Step 4. Compute correlations between PCA components and 
# all numeric variables in the big dataset
############################################################

# Select only numeric columns
numeric_vars <- merged_df %>%
  select(where(is.numeric))

# Extract just the PCA columns
pca_vars <- numeric_vars %>%
  select(PC1, PC2, PC_combined)

# Correlation matrix of all bigdf numeric variables vs PCA axes
cor_results <- sapply(names(numeric_vars), function(var) {
  sapply(names(pca_vars), function(pc) {
    cor(numeric_vars[[var]], pca_vars[[pc]], use = "pairwise.complete.obs")
  })
})

# Convert to tidy format for export/plotting
cor_df <- as.data.frame(cor_results)
cor_df$Variable <- rownames(cor_df)
rownames(cor_df) <- NULL
cor_df <- cor_df %>%
  relocate(Variable)

############################################################
# Step 5. Visualization: Heatmap of correlations
############################################################
# Melt for ggplot
library(reshape2)

cor_melt <- melt(cor_results)
colnames(cor_melt) <- c("Variable", "PC", "Correlation")

cor_melt <- cor_melt %>%
  slice(-c(1:12))  # remove first two rows

# Plot heatmap
ggplot(cor_melt, aes(x = PC, y = Variable, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Correlation of Variables with PCA Axes")



# Filter only PC_combined correlations
cor_combined <- cor_melt %>%
  filter(Variable == "PC_combined")

# Plot as a bar chart
ggplot(cor_combined, aes(x = PC, y = Correlation)) +
  geom_bar(stat = "identity") +
  theme_minimal(base_size = 12) +
  labs(title = "Correlation of Variables with PC_combined",
       x = "Variable",
       y = "Correlation") +
  coord_flip()   # optional, makes variables easier to read


############################################################
# Step 6. Save outputs
############################################################
# Export correlation table as CSV
write.csv(cor_df, "PCA_variable_correlations.csv", row.names = FALSE)
