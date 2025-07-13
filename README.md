# SManalyst: Spatial Metabolomics Analysis Platform

## Introduction

**SManalyst** is an innovative open-source platform for comprehensive analysis of spatial metabolomics data. It integrates quality control, metabolite annotation, spatial pattern discovery, and differential analysis into a unified workflow. Designed to address key challenges in spatial metabolomics, SManalyst enables researchers to explore metabolic heterogeneity in tissue microenvironments through an intuitive web interface.

## Key Features

### 🧪 **Integrated Analysis Workflow**

*   **Systematic Quality Control**:

    *   Background consistency evaluation (QC1)
    *   Noise ion filtering (QC2)
    *   Intensity distribution analysis (QC3)
    *   Missing value assessment (QC4)
*   **Advanced Metabolite Annotation**:

    *   Isotope/adduct identification with spatial correlation validation
    *   Multi-evidence scoring (mass accuracy, isotope pattern, adduct form)
    *   Support for custom databases and public repositories (HMDB/KEGG/LIPIDMAPS)

### 🔍 **Spatial Pattern Discovery**

*   **Metabolite-level clustering**: Group ions by spatial co-expression patterns
*   **Pixel-level clustering**: Identify tissue regions using 4 algorithms:

    *   Seurat-LV, Seurat-LM, Seurat-SLM, UMAP-kmeans

### ⚖️ **Flexible Differential Analysis**

*   **Automatic ROI generation** from clustering results
*   **Manual ROI selection** via interactive tissue imaging
*   Differential metabolite detection with fold-change/significance filters

### 📊 **Interactive Visualization**

*   Single/multi-ion imaging (RGB overlays)
*   Ion co-localization analysis


## Getting Started  

### 🌐 Online Access  
Access the live platform without installation:  
[SManalyst Web App](https://metax.genomics.cn/app/SManalyst)  

### 💻 Local Installation  
 **Prerequisites**:  
   - R (≥ v4.1.0)  
   - Required packages:  
     ```r
     # Install CRAN packages
     install.packages(c(
       "shinydashboard", "ggplot2", "shiny", "shinythemes", "tidyverse",
       "reshape2", "pheatmap", "shinyjs", "plotly", "ggnewscale", "cowplot",
       "readr", "dplyr", "magrittr", "plyr", "tidyr", "enviPat", "msentropy",
       "DT", "qvalue", "SpaGene", "data.table", "Matrix", "rjson", "RColorBrewer",
       "pbapply", "shinyBS", "sf", "spdep", "spatstat", "MetaboCoreUtils", "Seurat"
     ))
     
     # Install Bioconductor packages
     if (!require("BiocManager", quietly = TRUE))
         install.packages("BiocManager")
     
     BiocManager::install(c("Spectra", "glmGamPoi"))
     ```


### 📥 Input Data Format

SManalyst requires a **feature matrix** in CSV format:

*   **Columns 1-2**: Pixel coordinates (X, Y)
*   **Subsequent columns**: Ion intensities at each m/z value
*   **Row order**: Each row represents one spatial pixel

Example structure:

| X  | Y  | m/z\_100.002 | m/z\_101.005 | ... |
| :- | :- | :----------- | :----------- | :-- |
| 1  | 1  | 1500         | 0            | ... |
| 1  | 2  | 2400         | 310          | ... |

> 📖 Detailed formatting guide available in-app (Tutorial Panel → Data Preparation)

## Usage Workflow

1.  **Upload Data**: Import feature matrix via "Upload" tab
2.  **Quality Control**:

    *   Select tissue/background regions (interactive lasso tool)
    *   Adjust noise ion threshold (default: score ≥30)
    *   Review QC metrics in dashboard
3.  **Metabolite Annotation**:

    *   Select database (custom upload or built-in)
    *   Set mass error tolerance (ppm)
    *   Review annotation scores
4.  **Spatial Analysis**:

    *   Run metabolite/pixel clustering
    *   Compare regions: auto-clustered or manually drawn ROIs
5.  **Visualize & Export**:

    *   Explore ion distribution images
    *   Download results (CSV tables, PNG images)

## Example Data

Test dataset: **Mouse brain coronal section**

*   Acquisition: AFAD-ESI (+) mode, 100μm resolution
*   Content: 14,260 pixels × 3,044 ions
*   Access:

    *   Built-in dataset in SManalyst (Tutorial Panel → Example Data)
    *   Raw data: [NGDC OMIX Repository](https://ngdc.cncb.ac.cn/omix) ID: OMIX009541



## Community & Support

Developed by Zhanlong Mei (meizhanlong\@genomics.cn)\
Last update: 2025-07-13

