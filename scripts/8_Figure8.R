
# ==========================================================
# Figure 8A
# AMR gene mechanisms
# Stacked bar plot
# ====================================================

library(tidyverse)

# --------------------------
# Read data


amr <- read.csv(
  "amr_mechanisms.csv",
  stringsAsFactors = FALSE
)

# --------------------------
# Calculate totals


amr <- amr %>%
  mutate(
    Accessory_AMR =
      Inactivation +
      TargetAlteration
  )

# --------------------------
# Order isolates by total
# AMR gene count


amr <- amr %>%
  arrange(desc(Accessory_AMR))

amr$Isolate <- factor(
  amr$Isolate,
  levels = amr$Isolate
)

# --------------------------
# Convert to long format


amr_long <- amr %>%
  pivot_longer(
    cols = c(
      Efflux,
      Inactivation,
      TargetAlteration
    ),
    names_to = "Mechanism",
    values_to = "Count"
  )

# --------------------------
# Create figure


fig8a <- ggplot(
  amr_long,
  aes(
    x = Isolate,
    y = Count,
    fill = Mechanism
  )
) +
  
  geom_col(
    width = 0.8,
    colour = "white"
  ) +
  
  # total genes above bars
  geom_text(
    data = amr,
    aes(
      x = Isolate,
      y = Accessory_AMR,
      label = Accessory_AMR
    ),
    inherit.aes = FALSE,
    vjust = -0.4,
    fontface = "bold",
    size = 2
  ) +
  
  scale_fill_manual(
    values = c(
      "Efflux" = "#1f78b4",
      "Inactivation" = "#e6550d",
      "TargetAlteration" = "#b2182b"
    ),
    labels = c(
      "Efflux systems",
      "Antibiotic inactivation",
      "Target alteration"
    ),
    name = "AMR mechanism"
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.08)
    )
  ) +
  
  labs(
    x = NULL,
    y = "Number of AMR genes"
  ) +
  
  theme_bw(base_size = 10) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    legend.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank()
  )

# --------------------------
# Display


print(fig8a)

# --------------------------
# Export


ggsave(
  "Figure8A_AMRMechanisms.png",
  plot = fig8a,
  width = 8,
  height = 5,
  dpi = 600
)

# ==========================================================
# End Figure 8A
# =================================================
