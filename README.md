# paper-replication
R scripts for reproducing results

# Code for Microbiota–SII-PISA Analysis

This repository contains the R scripts used for statistical analyses and visualization in our study.

## Data availability
Due to ethical and privacy restrictions, the raw clinical and microbiome data are not publicly available.

## Genus anonymization
To protect sensitive microbiome information, all bacterial genera in the scripts have been anonymized using generic labels (e.g., `Genus_001`, `Genus_002`, etc.).

The mapping table linking real genus names to anonymized labels (`genus_mapping_PRIVATE.csv`) is **not included** in this repository.
Reviewers and editors may obtain this file from the authors upon reasonable request.

## Reproducibility
All scripts are provided exactly as used in the analysis.  
File names and sheet names are preserved, while absolute file paths are removed.

## Requirements
- R (>= 4.2.0)
- Packages listed at the top of each script
