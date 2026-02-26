#Importing measurements from Qupath
rawdata <- read.csv("/Volumes/lab-schmackk/home/shared/rawData/009_antibodyAssays/AbAs001_IRIS/AbAs001_1_IRIS_TBA24_CSF/TBA24_CSF/measurements2.csv")

#importing main metadata file 


#Make slide name and subject ID, and case category added 
library(dplyr)
library(stringr)
library(tidyr)

# slidenaming convention - 'TBAxx_yy_(OHC/OHS/PMS/PMC)_abcd'
# where xx - is TBA experiment number 
# yy is the actual slide number
# OHC/PMS whatever is projectcoede and csf/serum C or S
# abcd is the ID name 
# The coding works according to number of underscores - so do not change the naming convention

# If the naming convention is different will have to change this piece of code

rawdata <- rawdata %>%
  mutate(
    SlideID   = str_extract(Image, "^[^_]+_[^_]+"),          # first two chunks
    SubjectID = str_match(Image, "^[^_]+_[^_]+_[^_]+_([^_.]+)")[,2], # 4th chunk, before dot
    Group = case_when(
      str_detect(SubjectID, "a$") ~ "case",
      str_detect(SubjectID, "^[0-9]+$") ~ "control",
      TRUE ~ "exp_control"
    ),
    Group = factor(Group, levels = c("case", "control", "exp_control"))
  )

#Pivoting table ot usable format 


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
  pivot_wider(
    names_from = Classification,
    values_from = c(DAPI_mean, DAPI_stdev, FITC_mean, FITC_stdev, Cy5_mean, Cy5_stdev, Area),
    names_glue = "{Classification}_{.value}"
  )


#plotting
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
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
  filter(Group %in% c("case", "control"))   # remove exp_control

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


#Plotting the same for FITC for sanity check
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
  filter(Group %in% c("case", "control"))   # only keep case + control

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
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Prepare data
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

# Compute group means
group_means <- spider_df %>%
  group_by(Group, Classification) %>%
  summarise(Cy5_mean = mean(Cy5_mean, na.rm = TRUE), .groups = "drop")

# Spider plot with individuals (light) and group means (dark)
ggplot() +
  # individual subject traces
  geom_line(
    data = spider_df,
    aes(x = Classification, y = Cy5_mean, group = SubjectID, color = Group),
    alpha = 0.2, linewidth = 0.6
  ) +
  # group mean traces
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
