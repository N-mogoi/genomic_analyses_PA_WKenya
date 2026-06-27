
#INSTALL R Pckages

packages <- c(
  "tidyverse",
  "ComplexHeatmap",
  "circlize",
  "ggplot2",
  "ggpubr",
  "rstatix",
  "patchwork",
  "dendextend",
  "broom"
)

installed <- packages %in% rownames(installed.packages())

if(any(!installed)){
  install.packages(packages[!installed])
}

invisible(lapply(packages, library, character.only = TRUE))

##END##