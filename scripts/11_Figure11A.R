# ==========================================================
# QS Analysis 1
# Association between QS regulator genotype
# and quorum-sensing signal production
# ==========================================

# --------------------------
# Load packages


library(tidyverse)
library(rstatix)
library(ggpubr)

# --------------------------
# Read data


qs_raw <- read.csv(
  "QS_raw.csv",
  stringsAsFactors = FALSE
)

qs_meta <- read.csv(
  "QS_metadata.csv",
  stringsAsFactors = FALSE
)

# --------------------------
# Clean IDs


qs_raw$IsolateID  <- trimws(qs_raw$IsolateID)
qs_meta$IsolateID <- trimws(qs_meta$IsolateID)

# --------------------
# Merge


qs <- left_join(
  qs_raw,
  qs_meta,
  by = "IsolateID"
)

# -----------------------
# Remove reference strains


qs <- qs %>%
  filter(
    !IsolateID %in% c(
      "PAO1",
      "PA14"
    )
  )

# ------------------------
# Clean grouping variable


qs$QS_group <- case_when(
  tolower(qs$QS_group) == "intact" ~ "Intact",
  tolower(qs$QS_group) == "lof" ~ "LOF",
  TRUE ~ NA_character_
)

qs$QS_group <- factor(
  qs$QS_group,
  levels = c(
    "Intact",
    "LOF"
  )
)

# ----------------------------------
# Check merge


cat("\nSample numbers\n")

table(qs$QS_group)

table(
  qs$Signal,
  qs$QS_group
)

# ======================================================
# Summary statistics
# =============================================

summary_stats <- qs %>%
  group_by(
    Signal,
    QS_group
  ) %>%
  summarise(
    n = n(),
    Mean = mean(Luminescence),
    Median = median(Luminescence),
    SD = sd(Luminescence),
    SEM = SD / sqrt(n()),
    .groups = "drop"
  )

summary_stats

write.csv(
  summary_stats,
  "QS_Group_Summary.csv",
  row.names = FALSE
)

# ============================================
# Wilcoxon rank-sum tests
# ===========================================

wilcox_results <- qs %>%
  group_by(Signal) %>%
  wilcox_test(
    Luminescence ~ QS_group,
    exact = FALSE
  ) %>%
  adjust_pvalue(
    method = "BH"
  ) %>%
  add_significance()

wilcox_results

write.csv(
  wilcox_results,
  "QS_Wilcoxon_Results.csv",
  row.names = FALSE
)


####################################################################################################


#####################################################################################################
# ==========================================================
# Effect sizes

install.packages('coin')

effect_sizes <- qs %>%
  group_by(Signal) %>%
  wilcox_effsize(
    Luminescence ~ QS_group
  )

effect_sizes

write.csv(
  effect_sizes,
  "QS_Wilcoxon_EffectSize.csv",
  row.names = FALSE
)

# ==========================================================
# Publication-quality figure
# ====# ==========================================================
# P-value annotations
# ==================================================

pvals <- data.frame(
  Signal = c("BHL", "OdDHL", "PQS"),
  group1 = "Intact",
  group2 = "LOF",
  label = c(
    "Padj = 4.11 × 10^-8",
    "Padj = 4.11 × 10^-8",
    "ns (Padj = 0.228)"
  ),
  signif = c(
    "****",
    "****",
    "ns"
  ),
  y.position = c(
    395000,
    335000,
    1125
  )
)

#plot
p <- ggplot(
  qs,
  aes(
    x = QS_group,
    y = Luminescence,
    fill = QS_group
  )
) +
  
  geom_boxplot(
    width = 0.6,
    outlier.shape = NA
  ) +
  
  geom_jitter(
    width = 0.12,
    size = 2,
    alpha = 0.8
  ) +
  
  facet_wrap(
    ~Signal,
    scales = "free_y"
  ) +
  
  scale_fill_manual(
    values = c(
      "Intact" = "#1b9e77",
      "LOF" = "#d95f02"
    )
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold")
  ) +
  
  labs(
    y = "Luminescence"
  ) +
  
  stat_pvalue_manual(
    pvals,
    label = "label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    bracket.size = 0.5,
    size = 3.5
  ) +
  
  stat_pvalue_manual(
    pvals,
    label = "signif",
    xmin = "group1",
    xmax = "group2",
    y.position = pvals$y.position * c(0.97, 0.97, 0.985),
    tip.length = 0,
    bracket.size = 0,
    size = 5
  )

p

ggsave(
  "Figure_11A_Genotype_vs_Phenotype.png",
  p,
  width = 9,
  height = 6,
  dpi = 900
)

# ======================================================
# End
# ===========================================