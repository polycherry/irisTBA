library(dplyr)

#Step 1: Create a 4-level grouping variable

merged_df <- merged_df %>%
  mutate(group4 = case_when(
    group == 2 ~ "control",
    group == 1 & outlier ~ "case_outlier",
    group == 1 & !outlier ~ "case_non_outlier"
  ))

merged_df$group4 <- factor(merged_df$group4, levels = c("control", "case_non_outlier", "case_outlier"))

#Step 2: Reshape data to long format

library(dplyr)
library(haven)
library(tidyr)

# Convert all labelled columns to numeric (keep their underlying values)
merged_df_clean <- merged_df %>%
  mutate(across(where(haven::is.labelled), ~ as.numeric(.x)))

# Ensure group4 is a factor
merged_df_clean$group4 <- factor(merged_df_clean$group4, 
                                 levels = c("control", "case_non_outlier", "case_outlier"))

# Pivot longer safely, excluding group4
long_df <- merged_df_clean %>%
  pivot_longer(
    cols = 23:(ncol(merged_df_clean)-1),  # exclude group4
    names_to = "Variable",
    values_to = "Value"
  )

#plotting all numerical data based on the 3 gruops
library(ggplot2)

#barplot
ggplot(long_df, aes(x = group4, y = Value, fill = group4)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal(base_size = 12) +
  labs(title = "Biomarker values across groups",
       x = "Group",
       y = "Value")
#jitter plot 
ggplot(long_df, aes(x = group4, y = Value, color = group4)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal(base_size = 12) +
  labs(title = "Biomarker values across groups",
       x = "Group",
       y = "Value")



var_stats <- long_df %>%
  group_by(Variable, group4) %>%
  summarise(mean_val = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  group_by(Variable) %>%
  summarise(diff = max(mean_val) - min(mean_val)) %>%
  arrange(desc(diff))

# top 10 most different variables
top_vars <- head(var_stats$Variable, 10)

ggplot(filter(long_df, Variable %in% top_vars),
       aes(x = group4, y = Value, fill = group4)) +
  geom_jitter(aes(color = group4), width = 0.2, size = 1.5, alpha = 0.7) +  # adds dots
  stat_summary(fun = "mean", geom = "bar", color = "black", position = "dodge") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")  # optional to remove legend for group4

ggplot(filter(long_df, Variable %in% top_vars),
       aes(x = group4, y = Value, color = group4)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +  # only dots
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")  # optional


#adding IgG levels data
library(dplyr)

AbAs001_IRIS_ELISA_Serum_CSF_IgGlevel <- read_excel("/Volumes/lab-schmackk/home/shared/rawData/009_antibodyAssays/AbAs001_IRIS/AbAs001_5_IRIS_ELISA_IgG_all/AbAs001_IRIS_ELISA_Serum+CSF_IgGlevel.xlsx", 
                                                    +     sheet = "Results")

merged_df_clean <- merged_df_clean %>%
  left_join(
    AbAs001_IRIS_ELISA_Serum_CSF_IgGlevel %>% 
      select(`Sample ID`, CSF_mean, serum_mean),
    by = c("SubjectID" = "Sample ID")
  )

#now exporting it to save 
write.csv(merged_df, "merged_df_clean.csv", row.names = FALSE)




