################################################################################
# Clean Heatmap Visualization
# Purpose: Generate heatmaps of Cy5 mean fluorescence by brain region
################################################################################

library(pheatmap)
library(RColorBrewer)
library(viridis)

# Generate basic heatmap
pheatmap(t(heatmap_matrix),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         annotation_col = annotation_df,
         show_colnames = TRUE,
         main = "Heatmap of Cy5_mean per Classification")

# Define custom colors for group annotations
# Define custom colors for group annotations
ann_colors <- list(
  Group = c(
    case = "#FBB4AE",        # Soft pink
    control = "#B3CDE3",     # Soft blue
    exp_control = "#CCEBC5"  # Soft green
  ),
  Outlier = c(
    "TRUE" = "#bb9bbb",      # Light purple
    "FALSE" = "#E0E0E0"      # Light gray
  )
)

# Plot heatmap with custom annotation colors
pheatmap(t(heatmap_matrix),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         annotation_col = annotation_df,
         show_colnames = TRUE,
         main = "Heatmap of anti-human IgG MFI per brain area",
         annotation_colors = ann_colors)

# Create subset: cases and controls only
cols_to_keep <- feature_df$Group %in% c("case", "control")
heatmap_matrix_subset <- heatmap_matrix[cols_to_keep, ]
annotation_df_subset <- annotation_df[cols_to_keep, ]

# Plot subset with viridis color scheme
pheatmap(t(heatmap_matrix_subset),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         annotation_col = annotation_df_subset,
         show_colnames = TRUE,
         main = "Heatmap of anti-human IgG MFI per brain area",
         color = viridis(100),
         annotation_colors = ann_colors)

