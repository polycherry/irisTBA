################################################################################
# PCA Loadings Analysis
# Purpose: Examine PCA results, loadings, and create combined PC scores
################################################################################

library(ggplot2)
library(dplyr)

# Display PCA summary
# Display PCA summary
summary(pca_res)

################################################################################
# Visualize PCA results
################################################################################

ggplot(pca_df, aes(x = PC1, y = PC2, color = Group, shape = outlier)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "PCA: PC1 vs PC2",
       x = paste0("PC1 (", round(100*summary(pca_res)$importance[2,1], 1), "%)"),
       y = paste0("PC2 (", round(100*summary(pca_res)$importance[2,2], 1), "%)")) +
  theme_minimal()

################################################################################
# Extract and display top loadings
################################################################################

loadings <- pca_res$rotation[,1:2]
head(loadings[order(abs(loadings[,1]), decreasing = TRUE), ])  # Top contributors to PC1
head(loadings[order(abs(loadings[,2]), decreasing = TRUE), ])  # Top contributors to PC2

################################################################################
# Group mean PC scores
################################################################################

pca_df %>%
  group_by(Group) %>%
  summarise(mean_PC1 = mean(PC1), mean_PC2 = mean(PC2))

################################################################################
# Create weighted combined PC score
################################################################################

# Get proportion of variance explained
var_explained <- pca_res$sdev^2 / sum(pca_res$sdev^2)

# Weighted sum of PC1 and PC2
pca_df$PC_combined <- pca_df$PC1 * var_explained[1] + pca_df$PC2 * var_explained[2]

################################################################################
# Cytokine data PCA loadings (from pca2)
################################################################################

loadings2 <- pca2$rotation[,1:3]
head(loadings2[order(abs(loadings2[,1]), decreasing = TRUE), ])  # Top contributors to PC1
head(loadings2[order(abs(loadings2[,2]), decreasing = TRUE), ])  # Top contributors to PC2
head(loadings2[order(abs(loadings2[,3]), decreasing = TRUE), ])  # Top contributors to PC3
