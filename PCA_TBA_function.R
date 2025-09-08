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


### --- Run all 4 combinations ---

# 1. All groups, mean + stdev
pca_res1 <- run_pca(feature_df, include_exp = TRUE, include_stdev = TRUE, 
                    title = "PCA 1: All groups, Cy5 mean + stdev")

# 2. All groups, mean only
pca_res2 <- run_pca(feature_df, include_exp = TRUE, include_stdev = FALSE, 
                    title = "PCA 2: All groups, Cy5 mean only")

# 3. Case + Control only, mean + stdev
pca_res3 <- run_pca(feature_df, include_exp = FALSE, include_stdev = TRUE, 
                    title = "PCA 3: Case + Control, Cy5 mean + stdev")

# 4. Case + Control only, mean only
pca_res4 <- run_pca(feature_df, include_exp = FALSE, include_stdev = FALSE, 
                    title = "PCA 4: Case + Control, Cy5 mean only")
