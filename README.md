# GeneAnalyzeR Shiny App

[![DOI](https://zenodo.org/badge/739596335.svg)](https://zenodo.org/doi/10.5281/zenodo.10483599)

## URL
https://ewelch7.shinyapps.io/gene_expression_analyzer/

## Overview
This is a Shiny app designed to analyze gene expression data. It allows users to upload their data, view summary statistics, generate plots, and perform ANOVA analysis.

## Features
1. **Data Upload**: Users can upload their data in `.xlsx` format.
2. **Data Visualization**: 
  - Display raw data and summary statistics.
- Generate and download custom plots.
3. **Statistical Analysis**: Perform ANOVA analysis and post-hoc analysis.

## Getting Started

### Prerequisites
- R
- Shiny package
- Additional R packages: shinydashboard, shinyjs, readxl, dplyr, DT, ggplot2

### Installation
- Install the required R packages using `install.packages("packageName")`.

### Running the Application
- Run the app by executing `shinyApp(ui, server)` in R.

## Usage

1. **Upload Tab**: Upload an Excel file with gene expression data.
2. **Summary Tab**: View summary statistics in table format.
3. **Plot Tab**: Customize and view plots for the data.
4. **ANOVA Tab**: Choose the type of ANOVA analysis, run it, and view results.

## Contributions
Contributions to this project are welcome. Please follow the standard GitHub pull request process.

## License
This project is licensed under the MIT License - see the LICENSE file for details.

---
  

