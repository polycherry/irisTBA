################################################################################
# Cytokine PCA Analysis
# Purpose: Principal component analysis on different feature sets from merged
#          clinical and biomarker data
# Feature sets:
#   - Columns 14-42 (excluding 17, 20): Clinical/demographic features
#   - Columns 43-73 (excluding 71): Serum/CSF biomarkers
################################################################################

library(ggplot2)
library(dplyr)
library(tibble)

################################################################################
# PCA 1: Clinical and demographic features (columns 14-42, excluding 17 and 20)
################################################################################

pca1_data <- merged_df_clean %>%
  select(14:42) %>%
  select(-c(ethnicity_text, current_meds))  # Exclude non-numeric columns

# Run PCA with scaling
pca1 <- prcomp(pca1_data, scale. = TRUE)

# Create plotting dataframe with PC scores
pca1_df <- as_tibble(pca1$x[,1:2]) %>%
  mutate(group4 = merged_df_clean$group4)

# Visualize PC1 vs PC2
ggplot(pca1_df, aes(x = PC1, y = PC2, color = group4)) +
  geom_point(size=3) +
  theme_minimal() +
  labs(title = "PCA 1: Clinical / Demographic Features")

################################################################################
# PCA 2: Serum/CSF biomarkers (columns 43-73, excluding 71)
################################################################################
# PCA 2: Serum/CSF biomarkers (columns 43-73, excluding 71)
################################################################################

# Define column range for biomarkers
pca2_cols <- 43:73
# Exclude group4 column (71)
pca2_cols <- pca2_cols[pca2_cols != 71]

# Identify rows with complete cases (no missing values)
rows_keep <- complete.cases(merged_df_clean[, pca2_cols])

# Subset data and group labels
pca2_data_clean <- merged_df_clean[rows_keep, pca2_cols]
group4_clean <- merged_df_clean$group4[rows_keep]

# Run PCA
pca2 <- prcomp(pca2_data_clean, scale. = TRUE)

# Create tibble for plotting (using first 3 PCs)
pca2_df <- as_tibble(pca2$x[, 1:3]) %>%
  mutate(group4 = group4_clean)

# Plot PC1 vs PC3
ggplot(pca2_df, aes(x = PC1, y = PC3, color = group4)) +
  geom_point(size=3) +
  theme_minimal() +
  labs(title = "PCA 2: Serum / CSF Biomarkers")

# Add SubjectID to results
pca2_df <- pca2_df %>%
  mutate(SubjectID = merged_df_clean$SubjectID[rows_keep])

# View the first few rows
head(pca2_df)

################################################################################
# PCA 3: Combined features (columns 14-73)
################################################################################
# PCA 3: Combined features (columns 14-73)
################################################################################

# Select all features, excluding non-numeric columns
pca3_data <- merged_df_clean %>%
  select(14:73) %>%
  select(-c(age, current_meds)) 

# Run PCA
pca3 <- prcomp(pca3_data, scale. = TRUE)

# Create plotting dataframe
pca3_df <- as_tibble(pca3$x[,1:2]) %>%
  mutate(group4 = merged_df_clean$group4)

# Visualize combined features
ggplot(pca3_df, aes(x = PC1, y = PC2, color = group4)) +
  geom_point(size=3) +
  theme_minimal() +
  labs(title = "PCA 3: Combined Features")
