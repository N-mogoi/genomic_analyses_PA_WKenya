# ==========================================================
# QS Analysis 3
# Association between QS genotype and biofilm formation
# Figure 11C
# =====================================================

# --------------------------
# Load packages


library(tidyverse)
library(rstatix)
library(ggpubr)

# --------------------------
# Read data


biofilm <- read.csv(
  "Biofilm_formation_CV.csv",
  stringsAsFactors = FALSE
)

qs_meta <- read.csv(
  "QS_metadata.csv",
  stringsAsFactors = FALSE
)

# --------------------------
# Convert to long format


biofilm_long <- biofilm %>%
  pivot_longer(
    cols = Rep1:Rep3,
    names_to = "Replicate",
    values_to = "Biofilm_OD570"
  )

# --------------------------
# Merge metadata


biofilm_long <- biofilm_long %>%
  left_join(
    qs_meta,
    by = "IsolateID"
  )

# --------------------------
# Keep clinical isolates only


biofilm_long <- biofilm_long %>%
  filter(
    !IsolateID %in% c(
      "PAO1",
      "PA14"
    )
  )

# --------------------------
# Check sample sizes


table(biofilm_long$QS_group)

# =======================================================
# Wilcoxon rank-sum test
# ==================================================

wilcox_results <- wilcox_test(
  biofilm_long,
  Biofilm_OD570 ~ QS_group,
  exact = FALSE
)

wilcox_results

# ==========================================================
# Effect size


effect <- wilcox_effsize(
  biofilm_long,
  Biofilm_OD570 ~ QS_group
)

effect

# =========================================================
# Summary statistics


summary_stats <- biofilm_long %>%
  group_by(QS_group) %>%
  summarise(
    
    n = n(),
    
    Mean = mean(Biofilm_OD570),
    
    Median = median(Biofilm_OD570),
    
    SD = sd(Biofilm_OD570),
    
    SEM = SD / sqrt(n()),
    
    Minimum = min(Biofilm_OD570),
    
    Maximum = max(Biofilm_OD570),
    
    .groups = "drop"
    
  )

summary_stats

# --------------------------
# Export results
# ------------------------

write.csv(
  wilcox_results,
  "Biofilm_Wilcoxon.csv",
  row.names = FALSE
)

write.csv(
  effect,
  "Biofilm_EffectSize.csv",
  row.names = FALSE
)

write.csv(
  summary_stats,
  "Biofilm_GroupSummary.csv",
  row.names = FALSE
)

# ==========================================================
# Figure annotation


ann <- data.frame(
  
  group1 = "Intact",
  
  group2 = "LOF",
  
  y.position = max(biofilm_long$Biofilm_OD570) * 1.12,
  
  label = paste0(
    "italic(P) == ",
    signif(wilcox_results$p, 3)
  )
  
)
# ==========================================================
# Figure 11C


#standardise labels
biofilm_long$QS_group <- factor(
  biofilm_long$QS_group,
  levels = c("intact", "LOF"),
  labels = c("Intact", "LOF")
)


p_2 <- ggplot(
  
  biofilm_long,
  
  aes(
    
    x = QS_group,
    
    y = Biofilm_OD570,
    
    fill = QS_group
    
  )
  
) +
  
  geom_boxplot(
    
    width = 0.6,
    
    outlier.shape = NA
    
  ) +
  
  geom_jitter(
    
    width = 0.15,
    
    size = 2,
    
    alpha = 0.8
    
  ) +
  
  annotate(
    
    "segment",
    
    x = 1,
    xend = 2,
    
    y = 1.78,
    yend = 1.78,
    
    linewidth = 0.3
    
  ) +
  
  annotate(
    
    "segment",
    
    x = 1,
    xend = 1,
    
    y = 1.78,
    yend = 1.75,
    
    linewidth = 0.3
    
  ) +
  
  annotate(
    
    "segment",
    
    x = 2,
    xend = 2,
    
    y = 1.78,
    yend = 1.75,
    
    linewidth = 0.3
    
  ) +
  
  annotate(
    
    "text",
    
    x = 1.5,
    
    y = 1.81,
    
    label = paste0(
      "italic(P) == ",
      signif(wilcox_results$p, 3)
    ),
    
    parse = TRUE,
    
    size = 3
    
  ) +
  
  scale_fill_manual(
    
    values = c(
      
      "Intact" = "#1b9e77",
      
      "LOF" = "#d95f02"
      
    )
    
  ) +
  
  labs(
    
    x = "",
    
    y = expression("Biofilm biomass (OD"[570]*")")
    
  ) +
  
  theme_classic(
    
    base_size = 12
    
  ) +
  
  theme(
    
    legend.position = "none",
    
    axis.title = element_text(
      face = "bold"
    )
    
  )

p_2

ggsave(
  
  "Figure11C_QS_vs_Biofilm.png",
  
  p_2,
  
  width = 4.8,
  
  height = 4.5,
  
  dpi = 600
  
)

# ==========================================================
# End
# =======================================================