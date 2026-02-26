library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

df_summary$Group_New <- ifelse(seq_along(df_summary$Group) == 61, "Saline Control",
                               ifelse(seq_along(df_summary$Group) == 62, "Healthy Control",
                                      ifelse(seq_along(df_summary$Group) %in% 63:65, "Anti-NMDAR CSF Titration",
                                             ifelse(df_summary$Group == "case", "First Episode Psychosis",
                                                    ifelse(df_summary$Group == "control", "Healthy Control", df_summary$Group)))))


data <- df_summary %>%
  select(Image, Group_New, SubjectID, matches("Cy5_(mean|stdev)$")) %>%
  pivot_longer(
    cols = matches("Cy5_(mean|stdev)$"),
    names_to = c("Classification", ".value"),
    names_pattern = "(.*)_Cy5_(mean|stdev)$"
  ) %>%
  pivot_wider(names_from = Classification, values_from = c(mean, stdev))

features_matrix <- data %>%
  select(-Image, -Group_New, -SubjectID) %>%
  as.matrix()

features_scaled <- scale(features_matrix)



# PCA
pca_res <- prcomp(features_scaled, center = TRUE, scale. = TRUE)
pca_df <- data.frame(
  PC1 = pca_res$x[,1],
  PC2 = pca_res$x[,2],
  PC3 = pca_res$x[,3],
  Image = data$Image,
  SubjectID = data$SubjectID,
  Group = data$Group_New)

geom_text(data = subset(pca_df, outlier), aes(label = SubjectID), 
          vjust = -0.5, hjust = 0.5, size = 3)

plot1 = ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point(size = 3) +
  theme_bw() 

plot1

plot2 = ggplot(pca_df, aes(x = PC1, y = PC2, color = Group)) + geom_point(size = 2.5) + theme_bw() +
  labs(title = "PCA of CSF Tissue-Based \nAssay Signal Across Brain Regions \n") +
  scale_color_manual(values = c(
    "Healthy Control" = "#66C2A5",       # soft green-teal
    "First Episode Psychosis" = "#E6550D", # gentle coral
    "Anti-NMDAR CSF Titration" = "#8DA0FB", # soft blue
    "Saline Control" = "#E78AC3"          # soft purple-pink
    ))
plot2
  
