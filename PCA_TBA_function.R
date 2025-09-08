feature_df_Cy5 <- df_summary %>%
  select(Image, Group, SubjectID, matches("Cy5_(mean|stdev)$")) %>%
  pivot_longer(
    cols = matches("Cy5_(mean|stdev)$"),
    names_to = c("Classification", ".value"),
    names_pattern = "(.*)_Cy5_(mean|stdev)$"
  ) %>%
  pivot_wider(names_from = Classification, values_from = c(mean, stdev))


feature_df_Cy5$outlier <- outlier_idx

feature_df_Cy5 <- feature_df_Cy5 %>% filter(SubjectID != "167a")

feature_df_FITC <- df_summary %>%
  select(Image, Group, SubjectID, matches("FITC_(mean|stdev)$")) %>%
  pivot_longer(
    cols = matches("FITC_(mean|stdev)$"),
    names_to = c("Classification", ".value"),
    names_pattern = "(.*)_FITC_(mean|stdev)$"
  ) %>%
  pivot_wider(names_from = Classification, values_from = c(mean, stdev))

feature_df_FITC$outlier <- outlier_idx

### --- Function to run PCA with flexible options ---
run_pca <- function(data, include_exp = TRUE, include_stdev = TRUE, title = "PCA") {
  # Filter groups if exp controls excluded
  if (!include_exp) {
    data <- data %>%
      filter(Group %in% c("case", "control")) %>%
      droplevels()
  }
  
  # Select features (mean only, or mean+stdev)
  if (include_stdev) {
    features_matrix <- data %>%
      select(-Image, -Group, -SubjectID, -outlier) %>%
      as.matrix()
  } else {
    features_matrix <- data %>%
      select(Image, Group, SubjectID, outlier, matches("^mean_")) %>%
      select(-Image, -Group, -SubjectID, -outlier) %>%
      as.matrix()
  }
  
  # Scale
  features_scaled <- scale(features_matrix)
  
  
  
  # PCA
  pca_res <- prcomp(features_scaled, center = TRUE, scale. = TRUE)
  pca_df <- data.frame(
    PC1 = pca_res$x[,1],
    PC2 = pca_res$x[,2],
    PC3 = pca_res$x[,3],
    Image = data$Image,
    SubjectID = data$SubjectID,
    Group = data$Group,
    outlier = data$outlier
  )
  
  # Plot PCA
  print(
    ggplot(pca_df, aes(x = PC1, y = PC2, color = Group, shape = outlier)) +
      geom_point(size = 3) +
      geom_text(data = subset(pca_df, outlier), aes(label = SubjectID), 
                vjust = -0.5, hjust = 0.5, size = 3) +
      theme_bw() +
      labs(title = title)
  )
  
  # Scree plot
  scree_df <- data.frame(PC = 1:length(pca_res$sdev),
                         Variance = (pca_res$sdev^2) / sum(pca_res$sdev^2))
  print(
    ggplot(scree_df, aes(x = PC, y = Variance)) +
      geom_line() + geom_point() +
      theme_bw() +
      labs(title = paste("Scree Plot -", title),
           x = "Principal Component", y = "Proportion of Variance")
  )
  
  # Loadings (top contributors PC1 and PC2)
  cat("\nTop loadings for PC1:\n")
  print(head(pca_res$rotation[order(abs(pca_res$rotation[,1]), decreasing = TRUE), 1, drop=FALSE]))
  cat("\nTop loadings for PC2:\n")
  print(head(pca_res$rotation[order(abs(pca_res$rotation[,2]), decreasing = TRUE), 2, drop=FALSE]))
  
  return(pca_res)
}


### --- Run all 4 combinations for Cy5---

# 1. All groups, mean + stdev
pca_res1 <- run_pca(feature_df_Cy5, include_exp = TRUE, include_stdev = TRUE, 
                    title = "PCA 1: All groups, Cy5 mean + stdev")

# 2. All groups, mean only
pca_res2 <- run_pca(feature_df_Cy5, include_exp = TRUE, include_stdev = FALSE, 
                    title = "PCA 2: All groups, Cy5 mean only")

# 3. Case + Control only, mean + stdev
pca_res3 <- run_pca(feature_df_Cy5, include_exp = FALSE, include_stdev = TRUE, 
                    title = "PCA 3: Case + Control, Cy5 mean + stdev")

# 4. Case + Control only, mean only
pca_res4 <- run_pca(feature_df_Cy5, include_exp = FALSE, include_stdev = FALSE, 
                    title = "PCA 4: Case + Control, Cy5 mean only")


### --- Run all 4 combinations for FITC---

# 1. All groups, mean + stdev
pca_res5 <- run_pca(feature_df_FITC, include_exp = TRUE, include_stdev = TRUE, 
                    title = "PCA 1: All groups, FITC mean + stdev")

# 2. All groups, mean only
pca_res6 <- run_pca(feature_df_FITC, include_exp = TRUE, include_stdev = FALSE, 
                    title = "PCA 2: All groups, FITC mean only")

# 3. Case + Control only, mean + stdev
pca_res7 <- run_pca(feature_df_FITC, include_exp = FALSE, include_stdev = TRUE, 
                    title = "PCA 3: Case + Control, FITC mean + stdev")

# 4. Case + Control only, mean only
pca_res8 <- run_pca(feature_df_FITC, include_exp = FALSE, include_stdev = FALSE, 
                    title = "PCA 4: Case + Control, FITC mean only")





library(dplyr)
library(pheatmap)

# filter to cases + controls only
df_filtered <- feature_df_Cy5 %>%
  filter(Group %in% c("case", "control"))

# select only mean_* columns
mean_df <- df_filtered %>%
  select(starts_with("mean_"))

# correlation matrix
cor_matrix <- cor(mean_df, method = "pearson", use = "pairwise.complete.obs")

# heatmap
pheatmap(cor_matrix,
         main = "Correlation matrix (mean_* features only, cases + controls)",
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean")


library(dplyr)
library(pheatmap)
library(gridExtra)

# --- combine both groups to fix order ---
mean_all <- feature_df_Cy5 %>%
  filter(Group %in% c("case", "control")) %>%
  select(starts_with("mean_"))

cor_all <- cor(mean_all, method = "pearson", use = "pairwise.complete.obs")

# cluster once on all data
clust_rows <- hclust(dist(cor_all))
clust_cols <- hclust(dist(t(cor_all)))

# --- CASES ---
mean_cases <- feature_df_Cy5 %>%
  filter(Group == "case") %>%
  select(starts_with("mean_"))
cor_cases <- cor(mean_cases, method = "pearson", use = "pairwise.complete.obs")

p1 <- pheatmap(cor_cases,
               main = "Cases",
               cluster_rows = clust_rows,
               cluster_cols = clust_cols,
               silent = TRUE)

# --- CONTROLS ---
mean_controls <- feature_df_Cy5 %>%
  filter(Group == "control") %>%
  select(starts_with("mean_"))
cor_controls <- cor(mean_controls, method = "pearson", use = "pairwise.complete.obs")

p2 <- pheatmap(cor_controls,
               main = "Controls",
               cluster_rows = clust_rows,
               cluster_cols = clust_cols,
               silent = TRUE)

# --- side by side ---
gridExtra::grid.arrange(p1[[4]], p2[[4]], ncol = 2)


# difference matrix
cor_diff <- cor_cases - cor_controls

# plot difference heatmap
pheatmap(cor_diff,
         main = "Difference in correlations (Cases - Controls)",
         cluster_rows = clust_rows,
         cluster_cols = clust_cols,
         color = colorRampPalette(c("blue","white","red"))(100))




library(tidyr)
library(ggplot2)
library(ggrepel)

# compute subject-level average across all mean_* columns
subject_means <- df_filtered %>%
  mutate(subject_avg = rowMeans(select(., starts_with("mean_")), na.rm = TRUE))

# group-level averages
group_summary <- subject_means %>%
  group_by(Group) %>%
  summarise(mean_value = mean(subject_avg, na.rm = TRUE), .groups = "drop")

# plot
ggplot() +
  geom_col(data = group_summary,
           aes(x = Group, y = mean_value, fill = Group),
           width = 0.6, alpha = 0.6) +
  geom_point(data = subject_means,
             aes(x = Group, y = subject_avg, color = Group, shape = outlier),
             position = position_jitter(width = 0.15),
             size = 2) +
  ggrepel::geom_text_repel(data = subject_means %>% filter(outlier == TRUE),
                           aes(x = Group, y = subject_avg, label = SubjectID, color = Group),
                           position = position_jitter(width = 0.15),
                           size = 3) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17)) + # circle vs triangle
  theme_bw() +
  labs(title = "Average of all mean_* features per subject",
       x = "Group", y = "Subject average (across mean_* columns)")
