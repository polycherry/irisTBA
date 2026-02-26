library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(umap)
library(pheatmap)

# 1. Build feature matrix: one row per image, columns = Cy5_mean per classification
feature_df <- df_summary %>%
  select(Image, Group, SubjectID, matches("Cy5_(mean|stdev)$")) %>%
  pivot_longer(
    cols = matches("Cy5_(mean|stdev)$"),
    names_to = c("Classification", ".value"),
    names_pattern = "(.*)_Cy5_(mean|stdev)$"
  ) %>%
  pivot_wider(names_from = Classification, values_from = c(mean, stdev))

# 2. Optional: scale/center features
features_matrix <- feature_df %>%
  select(-Image, -Group, -SubjectID) %>%
  as.matrix()
features_scaled <- scale(features_matrix)

# 3. Identify outliers: images with any feature > mean + 2*sd
outlier_idx <- apply(features_scaled, 1, function(x) any(abs(x) > 2))
feature_df$outlier <- outlier_idx

# 4. PCA plot
pca_res <- prcomp(features_scaled, center = TRUE, scale. = TRUE)
pca_df <- data.frame(
  PC1 = pca_res$x[,1],
  PC2 = pca_res$x[,2],
  PC3 = pca_res$x[,3],
  Image = feature_df$Image,
  SubjectID = feature_df$SubjectID,
  Group = feature_df$Group,
  outlier = feature_df$outlier
)

ggplot(pca_df, aes(x = PC1, y = PC2, color = Group, shape = outlier)) +
  geom_point(size = 3) +
  geom_text(data = subset(pca_df, outlier), 
            aes(label = SubjectID), vjust = -0.5, hjust = 0.5, size = 3) +
  theme_bw() +
  labs(title = "PCA of Cy5_mean per Image")



#Repeat PCA without exp controls
feature_df_subset <- feature_df %>%
  filter(Group %in% c("case", "control")) %>%
  droplevels()

features_matrix2 <- feature_df_subset %>%
  select(-Image, -Group, -SubjectID, -outlier) %>%
  as.matrix()
features_scaled2 <- scale(features_matrix2)


pca_res2 <- prcomp(features_scaled2, center = TRUE, scale. = TRUE)
pca_df2 <- data.frame(
  PC1 = pca_res2$x[,1],
  PC2 = pca_res2$x[,2],
  PC3 = pca_res2$x[,3],
  Image = feature_df_subset$Image,
  SubjectID = feature_df_subset$SubjectID,
  Group = feature_df_subset$Group,
  outlier = feature_df_subset$outlier
)

ggplot(pca_df2, aes(x = PC1, y = PC2, color = Group, shape = outlier)) +
  geom_point(size = 3) +
  geom_text(data = subset(pca_df2, outlier), 
            aes(label = SubjectID), vjust = -0.5, hjust = 0.5, size = 3) +
  theme_bw() +
  labs(title = "PCA of Cy5_mean per Image")


#look for loadings of this PCA
loadings2 <- pca_res2$rotation[,1:2]
head(loadings2[order(abs(loadings2[,1]), decreasing = TRUE), ])  # Top contributors to PC1
head(loadings2[order(abs(loadings2[,2]), decreasing = TRUE), ])  # Top contributors to PC2

#scree plot 
# Scree plot in base R
plot(pca_res2, type = "l", main = "Scree Plot")

# 5. UMAP
set.seed(23)
umap_res <- umap(features_scaled)
umap_df <- data.frame(
  UMAP1 = umap_res$layout[,1],
  UMAP2 = umap_res$layout[,2],
  Image = feature_df$Image,
  SubjectID = feature_df$SubjectID,
  Group = feature_df$Group,
  outlier = feature_df$outlier
)

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Group, shape = outlier)) +
  geom_point(size = 3) +
  geom_text(data = subset(umap_df, outlier),
            aes(label = SubjectID), vjust = -0.5, hjust = 0.5, size = 3) +
  theme_bw() +
  labs(title = "UMAP of Cy5_mean per Image")

# 6. Heatmap of Cy5_mean per image

library(pheatmap)

# Make sure the matrix is numeric only
heatmap_matrix <- feature_df %>%
  select(-Image, -Group, -outlier, -SubjectID) %>%  # only Cy5_mean columns
  as.matrix() %>%
  apply(2, as.numeric)

# Set rownames
rownames(heatmap_matrix) <- feature_df$SubjectID

# Prepare annotation (must match rownames)
annotation_df <- data.frame(
  Group = factor(feature_df$Group),
  Outlier = factor(feature_df$outlier)
)
rownames(annotation_df) <- feature_df$SubjectID

# Plot
pheatmap(heatmap_matrix,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         annotation_row = annotation_df,
         show_rownames = TRUE,
         main = "Heatmap of Cy5_mean per Classification")



