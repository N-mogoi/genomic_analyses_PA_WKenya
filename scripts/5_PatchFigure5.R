

# ==========================================================
# Figure 5
# Virulence gene distribution
# Panel A: Virulence partitioning
# Panel B: GI-associated virulence genes
# ===================================================

library(patchwork)

# --------------------------
# Add panel labels


fig5a <- fig5a +
  labs(tag = "A") +
  theme(
    plot.tag = element_text(
      face = "bold",
      size = 18
    )
  )

fig5b <- fig5b +
  labs(tag = "B") +
  theme(
    plot.tag = element_text(
      face = "bold",
      size = 18
    )
  )

# --------------------------
# Combine panels


Figure5 <- fig5a + fig5b +
  
  plot_layout(
    widths = c(1.6, 1)
  )

# --------------------------
# Display


Figure5

# --------------------------
# Export


ggsave(
  "Figure5_VirulenceDistribution.png",
  plot = Figure5,
  width = 14,
  height = 6,
  dpi = 600
)

# ======================================
# End Figure 5
# ==============================================