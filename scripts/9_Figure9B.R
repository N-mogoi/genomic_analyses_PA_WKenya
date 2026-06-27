
# ==========================================================
# Figure 9B
# Distribution of selected Virulence genes
# ====================================================

# --------------------------
# Load packages
# -----------------

library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(grid)

# --------------------------
# Read Supplementary Table S2


vir <- read.csv(
  "Virulence_genes_S2.csv",
  skip = 6,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Extract isolate names from first row
new_names <- as.character(vir[1, ])

# Keep Gene/Product/Resistance names
new_names[1] <- "Gene"
new_names[length(new_names)-1] <- "Product"


# Apply new names
colnames(vir) <- new_names

# Remove header row
vir <- vir[-1, ]

# remove completely empty columns
vir <- vir[, colnames(vir) != ""]

# remove NA columns
vir <- vir[, !is.na(colnames(vir))]

#verifying
any(colnames(vir) == "")
any(is.na(colnames(vir)))

# Check
colnames(vir)

head(vir[,1:8])

# --------------------------
# Keep isolate columns only


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

amr_sub <- amr %>%
  select(
    Gene,
    all_of(iso_cols)
  )


#Keep the genes discussed in results
selected_genes <- c(
    
    # Alginate / biofilm
    "algR",
    "algZ",
    "algU",
    
    # Type III secretion
    "exoS",
    "exoT",
    "exoY",
    
    # Adhesion / pili
    "pilA",
    "pilY1",
    "pilP",
    "pilR",
    
    # Secreted toxins
    "aprA",
    "toxA",
    
    # Iron acquisition
    "pvdA",
    
    #Phenazine biosynethesis
    "phzH",
    
    #LPS/O-antigen regulation
    "wzz"
  )

#create vir subset
vir_fig <- vir %>%
  filter(Gene %in% selected_genes) %>%
  select(
    Gene,
    all_of(iso_cols)
  )


# --------------------------
# Convert:
# absent = 0
# chromosome = 1
# genomic island = 2
# -----------------

for(i in 2:ncol(vir_fig)){
  
  vir_fig[[i]] <- case_when(
    
    vir_fig[[i]] == "-" ~ 0,
    
    vir_fig[[i]] == "." ~ 0,
    
    vir_fig[[i]] == "C" ~ 1,
    
    grepl("GI", vir_fig[[i]]) ~ 2,
    
    TRUE ~ 0
  )
  
}

# --------------------------
# Create matrix
# -------------------

vir_mat <- as.matrix(
  vir_fig[, -1]
)

mode(vir_mat) <- "numeric"

rownames(vir_mat) <- vir_fig$Gene

colnames(vir_mat) <- c(
  "ID003","ID004","ID006","ID008",
  "ID009","ID011","ID012","ID013",
  "ID014","ID015","ID016","ID017",
  "ID020"
)

# --------------------------
# Colour palette
# ----------------------

location_cols <- c(
  
  "0" = "white",
  
  "1" = "#fdb863", #core chromosome
  
  "2" = "#b2182b" #Genomic island
  
)

# ------------------------------------------------
# Make isolates rows and genes columns
# -----------------------------------------

vir_mat <- t(vir_mat)

# ------------------------------------------------
# Order isolates according to Figure 7 phylogeny
# --------------------------------------------

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

vir_mat <- vir_mat[
  phylo_order,
  ,
  drop = FALSE
]

# --------------------------
# Heatmap
# -----------------

ht_vir <- Heatmap(
  
  vir_mat,
  
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
    
    title = "Virulence gene location",
    
    title_gp = gpar(
      fontface = "bold",
      fontsize = 10
    ),
    
    labels_gp = gpar(
      fontsize = 7
    ),
    
    at = c(0,1,2),
    
    labels = c("Absent", "Core chromosome", "Genomic island"),
    
    border = "black",
    
    grid_width = unit(2,"mm"),
    
    grid_height = unit(2,"mm")
  )
)

#draw
draw(ht_vir, heatmap_legend_side = "right")

# --------------------------
# Export publication figure


png(
  "Figure9B_Vir_Genes.png",
  width = 4000,
  height = 3600,
  res = 600
)

draw(
  ht_vir,
  heatmap_legend_side = "right"
)

dev.off()


# ==========================================================
# End Figure 9b
# =================================================