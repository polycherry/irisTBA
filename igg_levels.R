library(ggplot2)
library(dplyr)

# Create CSF/serum ratio
merged_df <- merged_df %>%
  mutate(CSF_serum_ratio = CSF_mean / serum_mean,
         # Create a new plotting group: control, case_non_outlier, case_outlier
         plot_group = case_when(
           group4 == "control" ~ "control",
           group4 == "case_non_outlier" ~ "case",
           group4 == "case_outlier" ~ "case"
         ))

# Function for bar + points + outlier triangles
plot_bar <- function(df, yvar, ylab) {
  ggplot(df, aes(x = plot_group, y = !!sym(yvar), fill = plot_group)) +
    geom_bar(stat = "summary", fun = "mean", width = 0.6, alpha = 0.6) +
    geom_jitter(width = 0.15, size = 2, color = "black") +
    geom_point(data = df %>% filter(outlier),
               aes(y = !!sym(yvar)), shape = 17, size = 3, color = "red") +
    geom_text(data = df %>% filter(outlier),
              aes(y = !!sym(yvar), label = SubjectID_clean),
              vjust = -1, size = 3) +
    labs(x = "", y = ylab) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Plot 1: serum_mean
plot_serum <- plot_bar(merged_df, "serum_mean", "Serum Mean")
plot_serum

# Plot 2: CSF_mean
plot_csf <- plot_bar(merged_df, "CSF_mean", "CSF Mean")
plot_csf

# Plot 3: CSF/Serum ratio
plot_ratio <- plot_bar(merged_df, "CSF_serum_ratio", "CSF / Serum Ratio")
plot_ratio
