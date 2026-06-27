# ==========================================================
# Figure 2
# Antibiotic susceptibility correlation analysis
# Spearman correlation coefficients and P-values
# ===================================================

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
# --------------------------
# Generate figure
# Show all correlations
# 

png(
  "Figure2_CorrelationMatrix.png",
  width = 4000,
  height = 3500,
  res = 600
)

par(
  mar = c(2, 2, 2, 2)
)

corrplot(
  cor_mat,
  
  method = "color",
  
  type = "upper",
  
  addCoef.col = "black",
  
  number.cex = 0.8,
  
  addgrid.col = "black",
  
  tl.col = "black",
  
  tl.srt = 45,
  
  tl.cex = 0.9,
  
  cl.cex = 1.1,
  
  diag = FALSE,
  
  col = colorRampPalette(
    c(
      "red",   # negative
      "white",     # neutral
      "green"    # positive
    )
  )(200)
  
)


dev.off()

#End of Figure 2

