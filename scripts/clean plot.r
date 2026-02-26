################################################################################
# Clean PCA Plot for Publication
# Purpose: Generate publication-ready PCA plot with custom group labels
################################################################################

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Create new group labels for visualization
df_summary$Group_New <- ifelse(seq_along(df_summary$Group) == 61, "Saline Control",
                               ifelse(seq_along(df_summary$Group) == 62, "Healthy Control",
                                      ifelse(seq_along(df_summary$Group) %in% 63:65, "Anti-NMDAR CSF Titration",
                                             ifelse(df_summary$Group == "case", "First Episode Psychosis",
                                                    ifelse(df_summary$Group == "control", "Healthy Control", df_summary$Group)))))

# Prepare feature matrix for PCA
data <- df_summary %>%
  select(Image, Group_New, SubjectID, matches("Cy5_(mean|stdev)$")) %>%
  pivot_longer(
    cols = matches("Cy5_(mean|stdev)$"),
    names_to = c("Classification", ".value"),
    names_pattern = "(.*)_Cy5_(mean|stdev)$"
  ) %>%
  pivot_wider(names_from = Classification, values_from = c(mean, stdev))

# Extract feature matrix and scale
features_matrix <- data %>%
  select(-Image, -Group_New, -SubjectID) %>%
  as.matrix()

features_scaled <- scale(features_matrix)

# Run PCA
pca_res <- prcomp(features_scaled, center = TRUE, scale. = TRUE)

# Create results dataframe
pca_df <- data.frame(
  PC1 = pca_res$x[,1],
  PC2 = pca_res$x[,2],
  PC3 = pca_res$x[,3],
  Image = data$Image,
  SubjectID = data$SubjectID,
  Group = data$Group_New)

# Basic PCA plot
plot1 = ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point(size = 3) +
  theme_bw() 

plot1

# Publication-ready PCA plot with custom colors
plot2 = ggplot(pca_df, aes(x = PC1, y = PC2, color = Group)) + 
  geom_point(size = 2.5) + 
  theme_bw() +
  labs(title = "PCA of CSF Tissue-Based \nAssay Signal Across Brain Regions \n") +
  scale_color_manual(values = c(
    "Healthy Control" = "#66C2A5",           # Soft green-teal
    "First Episode Psychosis" = "#E6550D",   # Gentle coral
    "Anti-NMDAR CSF Titration" = "#8DA0FB",  # Soft blue
    "Saline Control" = "#E78AC3"              # Soft purple-pink
    ))
plot2
  
