14 -42
17, 20

43-73 - 71


library(ggplot2)
library(dplyr)
library(tibble)

# 1️⃣ PCA on columns 14-42 (excluding 17 and 20)
pca1_data <- merged_df_clean %>%
  select(14:42) %>%
  select(-c(ethnicity_text, current_meds))  # columns 17 and 20

pca1 <- prcomp(pca1_data, scale. = TRUE)

pca1_df <- as_tibble(pca1$x[,1:2]) %>%
  mutate(group4 = merged_df_clean$group4)

ggplot(pca1_df, aes(x = PC1, y = PC2, color = group4)) +
  geom_point(size=3) +
  theme_minimal() +
  labs(title = "PCA 1: Clinical / Demographic Features")

# 2️⃣ PCA on columns 43-73 (excluding 71)
# Columns 43:73 in merged_df_clean
pca2_cols <- 43:73
# Exclude 71 (group4)
pca2_cols <- pca2_cols[pca2_cols != 71]

# Find rows with no NA in these columns in the original df
rows_keep <- complete.cases(merged_df_clean[, pca2_cols])

# Subset data and group4
pca2_data_clean <- merged_df_clean[rows_keep, pca2_cols]
group4_clean <- merged_df_clean$group4[rows_keep]

# Run PCA
pca2 <- prcomp(pca2_data_clean, scale. = TRUE)

# Create tibble for plotting
pca2_df <- as_tibble(pca2$x[, 1:3]) %>%
  mutate(group4 = group4_clean)

# Plot
ggplot(pca2_df, aes(x = PC1, y = PC3, color = group4)) +
  geom_point(size=3) +
  theme_minimal() +
  labs(title = "PCA 2: Serum / CSF Biomarkers")

# Add SubjectID to PCA2 tibble
pca2_df <- pca2_df %>%
  mutate(SubjectID = merged_df_clean$SubjectID[rows_keep])

# View the first few rows
head(pca2_df)



# 3️⃣ PCA on columns 14-73 (excluding char columns 17,20)
pca3_data <- merged_df_clean %>%
  select(14:73) %>%
  select(-c(age, current_meds)) 

pca3 <- prcomp(pca3_data, scale. = TRUE)

pca3_df <- as_tibble(pca3$x[,1:2]) %>%
  mutate(group4 = merged_df_clean$group4)

ggplot(pca3_df, aes(x = PC1, y = PC2, color = group4)) +
  geom_point(size=3) +
  theme_minimal() +
  labs(title = "PCA 3: Combined Features")
