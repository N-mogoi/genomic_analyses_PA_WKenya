# ==========================================================
# Figure 2
# Antibiotic susceptibility correlation analysis
# Spearman correlation coefficients and P-values
# ==================================================
# --------------------------
# Load packages
# -----------
library(tidyverse)
library(corrplot)
library(Hmisc)

# --------------------------
# Read continuous dataset

amr <- read.csv(
  "inhibition_diameters_continuous.csv",
  stringsAsFactors = FALSE
)

# --------------------------
# Create matrix containing
# inhibition-zone diameters
# ------------
amr_matrix <- amr %>%
  select(-Isolate)

# --------------------------
# Spearman correlation test
# ---------------
corr_results <- rcorr(
  as.matrix(amr_matrix),
  type = "spearman"
)

# --------------------------
# Extract correlation
# coefficients and P-values
# ---------
cor_mat <- corr_results$r
p_mat <- corr_results$P

# --------------------------
# Save complete outputs
# for reporting
# -------------
write.csv(
  round(cor_mat, 3), "Correlation_coefficients.csv")

write.csv(
  round(p_mat, 4),
  "Correlation_pvalues.csv"
)

# --------------------------
# Extract statistically
# significant correlations
# (P < 0.05)

sig_df <- data.frame()

for(i in 1:(ncol(cor_mat) - 1)) {
  
  for(j in (i + 1):ncol(cor_mat)) {
    
    if(!is.na(p_mat[i, j]) &&
       p_mat[i, j] < 0.05) {
      
      sig_df <- rbind(
        sig_df,
        data.frame(
          Antibiotic1 = colnames(cor_mat)[i],
          Antibiotic2 = colnames(cor_mat)[j],
          Rho = round(cor_mat[i, j], 3),
          Pvalue = signif(p_mat[i, j], 3)
        )
      )
      
    }
    
  }
  
}

# --------------------------
# Display significant pairs
# in console

sig_df

# --------------------
# Export significant pairs

write.csv(
  sig_df,
  "Significant_correlations.csv",
  row.names = FALSE
)

# --------------------------
#Colour palette:
#
# Negative correlations = blue
# No correlation         = white
# Positive correlations = orange
#
# This replaces the previous red-green palette.
# Blue-orange is more suitable for readers with
# red-green colour-vision deficiencies.
#
# The scale is also centred at zero so that positive
# and negative correlations are visually balanced.

correlation_palette <- colorRampPalette(
  c(
    "#2166AC",   # Blue: strong negative correlation
    "#F7F7F7",   # Very light grey/white: no correlation
    "#E66101"    # Orange: strong positive correlation
  )
)(200)

# --------------------------
# Save high-resolution figure
# ------------

png(
  "Figure2_CorrelationMatrix.png",
  width = 4000,
  height = 3500,
  res = 600
)

par(
  mar = c(2, 2, 2, 2))

# ------------------
# Correlation matrix
# --------------------------

corrplot(
  cor_mat,
  
  # Display correlations using colour
  method = "color",
  
  # Display only the upper triangle
  type = "upper",
  
  # Add numerical Spearman rho values
  # so that interpretation does not depend
  # on colour alone
  addCoef.col = "black",
  
  # Size of correlation coefficients
  number.cex = 0.8,
  
  # Add cell borders
  addgrid.col = "black",
  
  # Axis label colour
  tl.col = "black",
  
  # Rotate antibiotic labels
  tl.srt = 45,
  
  # Axis label size
  tl.cex = 0.9,
  
  # Colour legend text size
  cl.cex = 1.1,
  
  # Do not display the diagonal
  diag = FALSE,
  
  # Accessibility-friendly diverging palette
  col = correlation_palette,
  
  # Force the colour scale to run from
  # -1 (perfect negative correlation)
  # through 0 (no correlation)
  # to +1 (perfect positive correlation)
  cl.lim = c(-1, 1)
)


dev.off()

# End of Figure 2


