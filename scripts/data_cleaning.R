################################################################################
# Data Cleaning Script for TBA24 Analysis
# Purpose: Import and clean QuPath measurements, extract metadata from filenames,
#          and prepare data for downstream analysis
################################################################################

# Load required libraries
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Import raw measurements from QuPath
# Note: Update path to your local data location
rawdata <- read.csv("/Volumes/lab-schmackk/home/shared/rawData/009_antibodyAssays/AbAs001_IRIS/AbAs001_1_IRIS_TBA24_CSF/TBA24_CSF/measurements2.csv")

################################################################################
# Extract metadata from image filenames
################################################################################ 
# Extract metadata from image filenames
################################################################################

# Slide naming convention: 'TBAxx_yy_(OHC/OHS/PMS/PMC)_abcd'
# Where:
#   xx = TBA experiment number 
#   yy = Slide number
#   OHC/OHS/PMS/PMC = Project code and sample type (CSF or Serum)
#   abcd = Subject ID 
# 
# IMPORTANT: Parsing relies on underscore positions - maintain naming convention

rawdata <- rawdata %>%
  mutate(
    # Extract slide ID (first two chunks)
    SlideID   = str_extract(Image, "^[^_]+_[^_]+"),
    # Extract subject ID (4th chunk, before dot)
    SubjectID = str_match(Image, "^[^_]+_[^_]+_[^_]+_([^_.]+)")[,2],
    # Classify groups based on subject ID pattern
    Group = case_when(
      str_detect(SubjectID, "a$") ~ "case",              # IDs ending in 'a' are cases
      str_detect(SubjectID, "^[0-9]+$") ~ "control",     # Numeric IDs are controls
      TRUE ~ "exp_control"                               # Other IDs are experimental controls
    ),
    Group = factor(Group, levels = c("case", "control", "exp_control"))
  )

################################################################################
# Aggregate and reshape data
################################################################################ 
# Aggregate and reshape data
################################################################################

# Calculate mean intensity and standard deviation for each channel per classification
df_summary <- rawdata %>%
  mutate(Classification = as.factor(Classification)) %>%
  group_by(Image, SlideID, SubjectID, Group, Classification) %>%
  summarise(
    DAPI_mean  = mean(`ROI..2.00.µm.per.pixel..DAPI..Mean`, na.rm = TRUE),
    DAPI_stdev = mean(`ROI..2.00.µm.per.pixel..DAPI..Std.dev.`, na.rm = TRUE),
    FITC_mean  = mean(`ROI..2.00.µm.per.pixel..FITC..Mean`, na.rm = TRUE),
    FITC_stdev = mean(`ROI..2.00.µm.per.pixel..FITC..Std.dev.`, na.rm = TRUE),
    Cy5_mean   = mean(`ROI..2.00.µm.per.pixel..Cy5..Mean`, na.rm = TRUE),
    Cy5_stdev  = mean(`ROI..2.00.µm.per.pixel..Cy5..Std.dev.`, na.rm = TRUE),
    Area       = mean(`Area.µm.2`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Reshape to wide format with one column per Classification per metric
  pivot_wider(
    names_from = Classification,
    values_from = c(DAPI_mean, DAPI_stdev, FITC_mean, FITC_stdev, Cy5_mean, Cy5_stdev, Area),
    names_glue = "{Classification}_{.value}"
  )

################################################################################
# Visualization: Cy5 channel (anti-human IgG signal)
################################################################################
# Visualization: Cy5 channel (anti-human IgG signal)
################################################################################

# Prepare data for plotting: pivot Cy5 mean values by classification
plot_df <- df_summary %>%
  select(Image, SlideID, SubjectID, Group, ends_with("Cy5_mean")) %>%
  pivot_longer(
    cols = ends_with("Cy5_mean"),
    names_to = "Classification",
    values_to = "Cy5_mean"
  ) %>%
  mutate(
    Classification = str_remove(Classification, "_Cy5_mean$")
  ) %>%
  filter(Group %in% c("case", "control"))   # Exclude experimental controls

# Bar plot with individual points for Cy5 intensity
ggplot(plot_df, aes(x = Classification, y = Cy5_mean, fill = Group)) +
  stat_summary(fun = mean, geom = "bar", 
               position = position_dodge(width = 0.8), width = 0.7, alpha = 0.6) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.8), width = 0.3) +
  geom_jitter(aes(color = Group), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
              alpha = 0.7, size = 2) +
  theme_bw() +
  labs(y = "Cy5 Mean Intensity", x = "Classification") +
  scale_fill_manual(values = c("case" = "red", "control" = "blue")) +
  scale_color_manual(values = c("case" = "red", "control" = "blue"))

################################################################################
# Quality control: FITC channel (sanity check)
################################################################################
# Quality control: FITC channel (sanity check)
################################################################################

# Prepare FITC data for plotting
plot_df_fitc <- df_summary %>%
  select(Image, SlideID, SubjectID, Group, ends_with("FITC_mean")) %>%
  pivot_longer(
    cols = ends_with("FITC_mean"),
    names_to = "Classification",
    values_to = "FITC_mean"
  ) %>%
  mutate(
    Classification = str_remove(Classification, "_FITC_mean$")
  ) %>%
  filter(Group %in% c("case", "control"))

# Bar plot with individual points for FITC intensity
ggplot(plot_df_fitc, aes(x = Classification, y = FITC_mean, fill = Group)) +
  stat_summary(fun = mean, geom = "bar", 
               position = position_dodge(width = 0.8), width = 0.7, alpha = 0.6) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.8), width = 0.3) +
  geom_jitter(aes(color = Group), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
              alpha = 0.7, size = 2) +
  theme_bw() +
  labs(y = "FITC Mean Intensity", x = "Classification") +
  scale_fill_manual(values = c("case" = "red", "control" = "blue")) +
  scale_color_manual(values = c("case" = "red", "control" = "blue"))

################################################################################
# Spider plot: Regional pattern visualization
################################################################################

# Spider plot: Regional pattern visualization
################################################################################

# Prepare data for spider plot
spider_df <- df_summary %>%
  select(Image, SlideID, SubjectID, Group, ends_with("Cy5_mean")) %>%
  pivot_longer(
    cols = ends_with("Cy5_mean"),
    names_to = "Classification",
    values_to = "Cy5_mean"
  ) %>%
  mutate(
    Classification = str_remove(Classification, "_Cy5_mean$")
  ) %>%
  filter(Group %in% c("case", "control"))

# Compute group means for overlay
group_means <- spider_df %>%
  group_by(Group, Classification) %>%
  summarise(Cy5_mean = mean(Cy5_mean, na.rm = TRUE), .groups = "drop")

# Create spider plot showing individual subject traces (light) and group means (dark)
ggplot() +
  # Individual subject traces
  geom_line(
    data = spider_df,
    aes(x = Classification, y = Cy5_mean, group = SubjectID, color = Group),
    alpha = 0.2, linewidth = 0.6
  ) +
  # Group mean traces
  geom_line(
    data = group_means,
    aes(x = Classification, y = Cy5_mean, group = Group, color = Group),
    linewidth = 1.2
  ) +
  geom_point(
    data = group_means,
    aes(x = Classification, y = Cy5_mean, color = Group),
    size = 2
  ) +
  coord_polar() +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_manual(values = c("case" = "red", "control" = "blue")) +
  labs(
    title = "Cy5 Mean Intensity (Spider Plot)",
    x = NULL,
    y = "Mean Intensity"
  )
