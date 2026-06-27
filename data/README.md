# Data

This directory is reserved for the input datasets required to reproduce the analyses described in the accompanying manuscript.

## Data availability

The complete genome sequences generated in this study are publicly available through the National Center for Biotechnology Information (NCBI) GenBank under accession numbers:

**CP172848–CP172860**

The phenotypic datasets, processed genomic annotations, and supplementary tables used in this study are available in the Supplementary Information accompanying the manuscript.

Because some datasets were derived from supplementary files or public repositories, they are not duplicated in this GitHub repository.

## Expected input files

The R scripts expect input files in comma-separated values (`.csv`) format. These include, but are not limited to:

- Antimicrobial susceptibility datasets
- Genome annotation tables
- Antimicrobial resistance gene tables
- Virulence gene tables
- Quorum-sensing datasets
- Biofilm assay datasets

Input filenames are specified within the corresponding R scripts.

## Reproducibility

After obtaining the required datasets from the manuscript Supplementary Information and NCBI, place them in this directory (or update the file paths within the scripts) before running the analyses.

For software dependencies, see `environment.yml`.
