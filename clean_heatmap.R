pheatmap(t(heatmap_matrix),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         annotation_col = annotation_df,   # now annotations correspond to columns
         show_colnames = TRUE,
         main = "Heatmap of Cy5_mean per Classification")

library(RColorBrewer)
library(viridis)


# Define custom colors for annotation
ann_colors <- list(
  Group = c(
    case = "#FBB4AE",        # soft pink
    control = "#B3CDE3",     # soft blue
    exp_control = "#CCEBC5"  # soft green
  ),
  Outlier = c(
    "TRUE" = "#bb9bbb",      # light orange
    "FALSE" = "#E0E0E0"      # light gray
  )
)

# Plot heatmap with nicer annotation colors
pheatmap(t(heatmap_matrix),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         annotation_col = annotation_df,   # annotations now on columns
         show_colnames = TRUE,
         main = "Heatmap of anti-human IgG MFI per brain area",
         
         annotation_colors = ann_colors)


cols_to_keep <- feature_df$Group %in% c("case", "control")
heatmap_matrix_subset <- heatmap_matrix[cols_to_keep, ]
annotation_df_subset <- annotation_df[cols_to_keep, ]

pheatmap(t(heatmap_matrix_subset),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         annotation_col = annotation_df_subset,
         show_colnames = TRUE,
         main = "Heatmap of anti-human IgG MFI per brain area",
         color = viridis(100),annotation_colors = ann_colors)

