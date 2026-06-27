# ==========================================================
# Figure 9A
# Distribution of selected AMR genes
# Isolates ordered according to Figure 7 phylogeny
# ==================================================

# --------------------------
# Load packages
# -

library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(grid)

# --------------------------
# Read Supplementary Table S1
# -------------------

amr <- read.csv(
  "AMR_genes_S1.csv",
  skip = 6,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Extract isolate names from first row
new_names <- as.character(amr[1, ])

new_names[1] <- "Gene"
new_names[length(new_names)-1] <- "Product"
new_names[length(new_names)] <- "Resistance"

colnames(amr) <- new_names

# Remove header row
amr <- amr[-1, ]

# --------------------------
# Keep isolate columns only
# --------------------

iso_cols <- c(
  "238627E_ID003",
  "238628E_ID004",
  "238629E_ID006",
  "238630E_ID008",
  "238631E_ID009",
  "238632E_ID011",
  "238633E_ID012",
  "238634E_ID013",
  "238635E_ID014",
  "238636E_ID015",
  "238637E_ID016",
  "238638E_ID017",
  "238641E_ID020"
)

# --------------------------
# Selected AMR genes


selected_genes <- c(
  "APH(3'')-Ib",
  "APH(3')-IIb",
  "APH(3')-Ia",
  "APH(6)-Id",
  "CrpP",
  "MexE",
  "MexF",
  "OprN",
  "catB7",
  "PDC-1",
  "PDC-10",
  "PmrB",
  "PmpM"
)

amr_fig <- amr %>%
  filter(Gene %in% selected_genes) %>%
  select(
    Gene,
    all_of(iso_cols)
  )

# --------------------------
# Convert locations
# absent = 0
# chromosome = 1
# genomic island = 2


for(i in 2:ncol(amr_fig)){
  
  amr_fig[[i]] <- case_when(
    
    amr_fig[[i]] == "-" ~ 0,
    
    amr_fig[[i]] == "." ~ 0,
    
    amr_fig[[i]] == "C" ~ 1,
    
    grepl("GI", amr_fig[[i]]) ~ 2,
    
    TRUE ~ 0
  )
  
}

# --------------------------
# Create matrix
# ---------------

amr_mat <- as.matrix(
  amr_fig[, -1]
)

mode(amr_mat) <- "numeric"

rownames(amr_mat) <- amr_fig$Gene

colnames(amr_mat) <- c(
  "ID003",
  "ID004",
  "ID006",
  "ID008",
  "ID009",
  "ID011",
  "ID012",
  "ID013",
  "ID014",
  "ID015",
  "ID016",
  "ID017",
  "ID020"
)

# genes as columns
# isolates as rows

amr_mat <- t(amr_mat)

# Fix PAO1 name if needed
rownames(amr_mat)[rownames(amr_mat) == "PA01"] <- "PAO1"

# --------------------------
# Order isolates according
# to Figure 7 phylogeny
# -------------------
# Figure 7 top-to-bottom order

phylo_order <- c(
  "ID016",
  "ID014",
  "ID015",
  "ID012",
  "ID008",
  "ID017",
  "ID003",
  "ID013",
  "ID011",
  "ID009",
  "ID004",
  "ID020",
  "ID006"
)

amr_mat <- amr_mat[
  phylo_order,
  ,
  drop = FALSE
]

# --------------------------
# Colour palette
# ------------------

location_cols <- c(
  
  "0" = "white",
  
  "1" = "#fdb863",
  
  "2" = "#b2182b"
  
)

# --------------------------
# Heatmap
# ----------------------

ht_amr <- Heatmap(
  
  amr_mat,
  
  name = "Location",
  
  col = location_cols,
  
  cluster_rows = FALSE,
  
  cluster_columns = FALSE,
  
  rect_gp = gpar(
    col = "black",
    lwd = 0.8
  ),
  
  row_names_side = "left",
  
  row_names_gp = gpar(
    fontsize = 8
  ),
  
  column_names_gp = gpar(
    fontsize = 7,
    fontface = "italic"
  ),
  
  column_names_side = "top",
  
  column_names_rot = 90,
  
  width = unit(3, "cm"),
  
  height = unit(7, "cm"),
  
  heatmap_legend_param = list(
    
    title = "AMR gene location",
    
    title_gp = gpar(
      fontface = "bold",
      fontsize = 10
    ),
    
    labels_gp = gpar(
      fontsize = 7
    ),
    
    at = c(0, 1, 2),
    
    labels = c(
      "Absent",
      "Core chromosome",
      "Genomic island"
    ),
    
    border = "black",
    
    grid_width = unit(2, "mm"),
    
    grid_height = unit(2, "mm")
    
  )
  
)

# --------------------------
# Draw
# -------------------

draw(
  ht_amr,
  heatmap_legend_side = "right"
)

# --------------------------
# Export publication figure
# ---------------------

png(
  "Figure9A_AMR_Genes.png",
  width = 4000,
  height = 3600,
  res = 600
)

draw(
  ht_amr,
  heatmap_legend_side = "right"
)

dev.off()

# ====================================================
# End Figure 9A
# ==========================================================