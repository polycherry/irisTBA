################################################################################
# Hemoglobin Contamination Analysis
# Purpose: Analyze correlation between hemoglobin (Hb) levels in CSF samples
#          and PCA scores to assess potential blood contamination effects
################################################################################

library(ggplot2)
library(ggrepel)
library(ggpubr)
library(dplyr)

# Import hemoglobin optical density (OD) measurements from ELISA
IRIS_samples_Hb_OD <- read_excel("/Volumes/lab-schmackk/home/shared/rawData/009_antibodyAssays/AbAs001_IRIS/AbAs001_7_CSF_Hb_ELISA/IRIS samples Hb OD.xlsx")

# Merge Hb OD data with PCA results
pca_df2 <- pca_df %>%
  left_join(IRIS_samples_Hb_OD, by = c("SubjectID" = "Sample"))

################################################################################
# Correlation analysis: PC1 vs Hemoglobin OD
################################################################################

# Basic scatterplot with regression line
ggplot(pca_df2, aes(x = PC1, y = OD, color = Group)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Correlation of PC1 with OD",
       x = "PC1",
       y = "OD")

# Pearson correlation test across all samples
cor_test <- cor.test(pca_df2$PC1, pca_df2$OD, method = "pearson")

# Extract correlation coefficient and p-value
r_val <- round(cor_test$estimate, 2)
p_val <- signif(cor_test$p.value, 3)

# Enhanced plot with statistics and outlier labels
ggplot(pca_df2, aes(x = PC1, y = OD, color = Group)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Single regression line
  geom_text_repel(
    data = subset(pca_df2, outlier),
    aes(label = SubjectID),
    color = "black", size = 3
  ) +
  theme_minimal() +
  labs(title = "Correlation of PC1 with OD",
       subtitle = paste0("Pearson r = ", r_val, ", p = ", p_val),
       x = "PC1",
       y = "OD")

################################################################################
# Group comparison: Hemoglobin OD levels (Cases vs Controls)
################################################################################

# Parametric test: t-test
t_test <- t.test(OD ~ Group, data = pca_df2 %>% filter(Group %in% c("case", "control")))

p_val <- signif(t_test$p.value, 3)

# Barplot with mean ± SE and individual points
ggbarplot(pca_df2, x = "Group", y = "OD",
          add = c("mean_se"),              # Show mean ± SE
          fill = "Group",                  # color by group
          palette = c("case" = "#E64B35", "control" = "#4DBBD5"),
          add.params = list(size = 1.2)) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.7) +  # show individual points
  stat_compare_means(comparisons = list(c("case","control")), 
                     method = "t.test", label = "p.format") +
  labs(title = "OD comparison between Cases and Controls",
       subtitle = paste0("t-test p = ", p_val),
       x = "Group",
       y = "OD") +
  theme_minimal()

################################################################################
# Non-parametric test: Wilcoxon rank-sum (Mann-Whitney U)
################################################################################

# Restrict to case vs control only
df_case_control <- pca_df2 %>% filter(Group %in% c("case", "control"))

# Run Wilcoxon rank-sum test
wilcox_test <- wilcox.test(OD ~ Group, data = df_case_control)

wilcox_test

df_case_control <- pca_df2 %>% filter(Group %in% c("case", "control"))

library(ggpubr)
library(ggrepel)

df_case_control <- pca_df2 %>% filter(Group %in% c("case", "control"))

ggbarplot(df_case_control, x = "Group", y = "OD",
          add = "median_iqr",           # median ± IQR
          fill = "Group",
          palette = c("case" = "#E64B35", "control" = "#4DBBD5")) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.7) +
  stat_compare_means(method = "wilcox.test", 
                     comparisons = list(c("case","control")), 
                     label = "p.format") +
  geom_text_repel(
    data = subset(df_case_control, outlier),
    aes(label = SubjectID),
    size = 3,
    color = "black",
    nudge_y = 0.02
  ) +
  labs(title = "OD comparison between Cases and Controls (Wilcoxon)",
       x = "Group", y = "OD") +
  theme_minimal()


