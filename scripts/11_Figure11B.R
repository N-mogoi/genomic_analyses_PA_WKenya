# ==========================================================
# QS Analysis 2
# Association between QS genotype and QS phenotype
# Figure 11B
# ====================================================

# --------------------------
# Load packages


library(tidyverse)

# -----------------------
# Read metadata


qs <- read.csv(
  "QS_metadata.csv",
  stringsAsFactors = FALSE
)

# -------------------------
# Keep phenotype classes


qs <- qs %>%
  mutate(
    
    Phenotype = case_when(
      
      grepl(
        "BHL",
        QS_phenotype
      ) ~ "BHL+/OdDHL+/PQS+",
      
      grepl(
        "PQS",
        QS_phenotype
      ) ~ "PQS only",
      
      TRUE ~ "Other"
      
    )
    
  )

# Check
table(
  qs$QS_group,
  qs$Phenotype
)

# ==========================================================
# Fisher's exact test


cont_table <- table(
  qs$QS_group,
  qs$Phenotype
)

cont_table

fisher_res <- fisher.test(
  cont_table
)

fisher_res

# Odds ratio

fisher_res$estimate

# P value

fisher_res$p.value

# --------------------------
# Export contingency table
# -----------------------

write.csv(
  as.data.frame.matrix(cont_table),
  "QS_Genotype_vs_Phenotype_Table.csv"
)

# ==========================================================
# Prepare plotting data


plot_dat <- qs %>%
  count(
    QS_group,
    Phenotype
  ) %>%
  group_by(
    QS_group
  ) %>%
  mutate(
    
    Percent =
      100 * n / sum(n)
    
  )

plot_dat

# ==========================================================
# Figure 11B


p_1 <- ggplot(
  
  plot_dat,
  
  aes(
    
    x = QS_group,
    
    y = Percent,
    
    fill = Phenotype
    
  )
  
) +
  
  geom_col(
    
    width = 0.7,
    
    colour = "black"
    
  ) +
  
  geom_text(
    
    aes(
      
      label = paste0(
        n,
        "\n(",
        round(Percent),
        "%)"
      )
      
    ),
    
    position = position_stack(
      vjust = 0.5
    ),
    
    size = 4
    
  ) +
  
  scale_fill_manual(
    
    values = c(
      
      "BHL+/OdDHL+/PQS+" = "#1b9e77",
      
      "PQS only" = "#d95f02"
      
    )
    
  ) +
  
  annotate(
    "text",
    x = 1.5,
    y = 110,
    label = "Fisher's exact test",
    size = 5,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = 1.5,
    y = 104,
    label = paste0(
      "italic(P) == ",
      signif(fisher_res$p.value, 3)
    ),
    parse = TRUE,
    size = 2
  ) +
  
  labs(
    
    x = "",
    
    y = "Isolates (%)",
    
    fill = "QS phenotype"
    
  ) +
  
  coord_cartesian(
    
    ylim = c(
      0,
      115
    )
    
  ) +
  
  theme_classic(
    
    base_size = 12
    
  ) +
  
  theme(
    
    legend.position = "right",
    
    axis.title = element_text(
      face = "bold"
    ),
    
    legend.title = element_text(
      face = "bold"
    )
    
  )

p_1

ggsave(
  
  "Figure11B_QS_Genotype_vs_Phenotype.png",
  
  p_1,
  
  width = 7,
  
  height = 5,
  
  dpi = 600
  
)

# ==========================================================
# End
# ========================================================