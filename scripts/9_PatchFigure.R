# ==========================================================
# Figure 9
# Combined AMR and virulence heatmaps
# ===============================================

library(ComplexHeatmap)
library(grid)
library(gridExtra)

# --------------------------
# Capture Figure 9A


grob_amr <- grid.grabExpr(
  draw(
    ht_amr,
    heatmap_legend_side = "right"
  )
)

# --------------------------
# Capture Figure 9B


grob_vir <- grid.grabExpr(
  draw(
    ht_vir,
    heatmap_legend_side = "right"
  )
)

# --------------------------
# Export combined figure


png(
  "Figure9_Combined.png",
  width = 4000,
  height = 3200,
  res = 600
)

grid.arrange(
  
  arrangeGrob(
    grobTree(
      textGrob(
        "(A)",
        x = unit(0.02, "npc"),
        y = unit(0.9, "npc"),
        just = c("left", "top"),
        gp = gpar(
          fontsize = 12,
          fontface = "bold"
        )
      ),
      grob_amr
    )
  ),
  
  arrangeGrob(
    grobTree(
      textGrob(
        "(B)",
        x = unit(0.02, "npc"),
        y = unit(0.9, "npc"),
        just = c("left", "top"),
        gp = gpar(
          fontsize = 12,
          fontface = "bold"
        )
      ),
      grob_vir
    )
  ),
  
  ncol = 2,
  
  widths = c(1, 1)
  
)

dev.off()

#--------------------------------------------

#END