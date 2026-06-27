# ==========================================================
# Figure 1A
# Antimicrobial susceptibility heatmap
# Continuous inhibition diameters used for clustering/color
# CLSI categories (R/I/S) displayed inside cells
# =====================================================

# --------------------------
# Load packages

library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(grid)
library(tibble)

# --------------------------
# Set working directory
# --------------------------
getwd()
# --------------------------
# Read continuous data
# (diameters in mm)

amr <- read.csv("inhibition_diameters_continuous.csv", stringsAsFactors = FALSE)

# --------------------------
# Read categorical data
# (R / I / S)

cat_df <- read.csv("inhibition_diameters_categorical.csv", stringsAsFactors = FALSE)

# --------------------------
# Create continuous matrix
# used for clustering

amr_matrix <- amr %>%
  column_to_rownames("Isolate") %>%
  as.matrix()

# ----------------------
# Create categorical matrix
# displayed inside cells

cat_matrix <- cat_df %>%
  column_to_rownames("Isolate") %>%
  as.matrix()



#----------------------------------------
# ---------------------
# Calculate resistance score
# Number of antibiotics
# classified as resistant


mdr_score <- apply(
  cat_matrix,
  1,
  function(x) sum(x == "R")
)


# --------------------------
# MLST information
# match isolate order

st <- c(
  "645",
  "870",
  "unknown",
  "16",
  "2132",
  "244",
  "856",
  "244",
  "274",
  "274",
  "274",
  "2305",
  "unknown"
)
# --------------------------
# Color function
# Continuous inhibition zones
col_fun <- colorRamp2(
  c(10, 20, 27),
  c(
    "#d73027",  # low diameter
    "#fee08b",  # intermediate
    "#1a9850"   # high diameter
  )
)

# --------------------------
# ST annotation colors

st_cols <- c(
  "16"      = "#1b9e77",
  "244"     = "#d95f02",
  "274"     = "#7570b3",
  "645"     = "#e7298a",
  "856"     = "#66a61e",
  "870"     = "#e6ab02",
  "2132"    = "#a6cee3",
  "2305"    = "#999999",
  "unknown" = "#000000"
)

# --------------------------
# Row annotation
# ----------------------
# --------------------
# Row annotations
# ST + MDR score
# --------------------------

ha <- rowAnnotation(
  
  ST = st,
  
  MDR = anno_text(
    paste0(mdr_score),
    width = unit(8, "mm"),
    gp = gpar(
      fontsize = 10,
      fontface = "bold"
    ),
    just = "center",
    location = 0.5
  ),
  
  col = list(
    ST = st_cols
  ),
  
  annotation_name_side = "top",
  
  annotation_name_gp = gpar(
    fontsize = 12,
    fontface = "bold"
  )
)

# --------------------------
# Build heatmap

ht <- Heatmap(
  
  amr_matrix,
  
  name = "Diameter\n(mm)",
  
  col = col_fun,
  
  cluster_rows = TRUE,
  cluster_columns = FALSE,
  
  left_annotation = ha,
  
  row_names_side = "right",
  
  column_names_rot = 45,
  
  border = TRUE,
  
  rect_gp = gpar(
    col = "white",
    lwd = 1
  ),
  
  # ----------------------
  # Display R/I/S values
  # inside cells
 
  cell_fun = function(
    j, i,
    x, y,
    width, height,
    fill
  ) {
    
    grid.text(
      cat_matrix[i, j],
      x = x,
      y = y,
      gp = gpar(
        fontsize = 12,
        fontface = "plain"
      )
    )
    
  },
  
  # ----------------------
  # Heatmap legend

  heatmap_legend_param = list(
    
    title = "Zone diameter (mm)",
    
    at = c(
      10,
      15,
      20,
      25
    ),
    
    labels = c(
      "10",
      "15",
      "20",
      "25"
    )
    
  )
)

# --------------------------
# high-resolution PNG

png(
  "Figure1A_ComplexHeatmap.png",
  width = 4000,
  height = 3000,
  res = 600
)

draw(
  ht,
  heatmap_legend_side = "right",
  annotation_legend_side = "right"
)

dev.off()

# ==========================================================
# End Figure 1A