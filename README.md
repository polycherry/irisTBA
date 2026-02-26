# TBA24 Analysis

Analysis pipeline for Tissue-Based Assay (TBA) immunofluorescence data examining autoantibody binding patterns in brain tissue.

## Overview

This repository contains R scripts for analyzing and visualizing tissue-based assay data comparing autoantibody binding across different brain regions in cases and controls. The pipeline includes data preprocessing, principal component analysis (PCA), correlation analysis, and visualization.

## Project Structure

```
TBA24_analysis/
├── scripts/           # R analysis scripts
│   └── archive/      # Older/exploratory scripts
├── data/             # Data files (not tracked in git)
├── plots/            # Output plots (not tracked in git)
└── README.md
```

## Analysis Pipeline

### 1. Data Cleaning and Preparation
**Script:** `data_cleaning.R`
- Imports QuPath measurement data
- Extracts metadata from image filenames
- Aggregates fluorescence intensities by brain region
- Creates exploratory visualizations (bar plots, spider plots)

### 2. PCA Analysis
**Scripts:** `PCA_TBA_function.R`, `cytokine PCA.r`
- Flexible PCA function for different feature combinations
- Separate analyses for Cy5 (IgG) and FITC channels
- Options to include/exclude experimental controls
- Multiple PCAs on clinical, biomarker, and combined features

### 3. Correlation Analysis
**Scripts:** `cytokine PANSS correlation.R`, `correlationsgraph.R`
- Merges PCA results with clinical datasets
- Calculates correlations between PCA axes and clinical variables
- Generates correlation heatmaps

### 4. IgG Level Analysis
**Script:** `igg_levels.R`
- Analyzes serum and CSF IgG levels
- Calculates CSF/serum ratios
- Identifies and visualizes outliers

### 5. Hemoglobin Contamination Analysis
**Script:** `hb samples.r`
- Correlates hemoglobin (Hb) optical density with PCA scores
- Tests for blood contamination effects in CSF samples
- Compares Hb levels between cases and controls using parametric and non-parametric tests

### 6. Visualization
**Scripts:** `clean plot.r`, `clean_heatmap.R`, `PCA_loadings.R`
- Publication-ready PCA plots with custom colors
- Heatmaps of fluorescence intensities by brain region
- PCA loading analysis and interpretation

## Requirements

### R Packages
```r
# Data manipulation
library(dplyr)
library(tidyr)
library(stringr)

# Visualization
library(ggplot2)
library(pheatmap)
library(viridis)
library(RColorBrewer)
library(ggrepel)
library(gridExtra)

# Analysis
library(reshape2)
library(tibble)
```

## Usage

1. **Set up data paths**: Update the file path in `data_cleaning.R` to point to your local data directory
2. **Run data cleaning**: Execute `data_cleaning.R` to process raw QuPath data
3. **Perform PCA**: Run `PCA_TBA_function.R` for comprehensive PCA analysis
4. **Analyze correlations**: Execute correlation scripts to link PCA results with clinical variables
5. **Generate visualizations**: Run visualization scripts to create publication-ready figures

## Data Format

### Input Files
- **QuPath measurements**: CSV file containing ROI measurements with columns for DAPI, FITC, and Cy5 channels
- **Clinical data**: Dataset with subject demographics and biomarker measurements

### Image Naming Convention
Images should follow this naming pattern: `TBAxx_yy_PROJECT_ID`
- `xx`: Experiment number
- `yy`: Slide number
- `PROJECT`: Project code (e.g., OHC, PMS)
- `ID`: Subject identifier

## Output

- PCA plots showing group separation
- Heatmaps of regional fluorescence patterns
- Correlation matrices
- Bar plots with individual data points
- Spider/radar plots for regional patterns

## Notes

- Outlier detection is performed on PCA results
- Group classifications: case, control, exp_control
- Both mean and standard deviation features can be included in analyses

## Author

Analysis by Vasishta

## License

This project is for research purposes.
