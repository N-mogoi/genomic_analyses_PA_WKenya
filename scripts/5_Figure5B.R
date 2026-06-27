
# ==========================================================
# Figure 5B
# GI-associated virulence genes
# Ranked bar plot
# ======================================================

library(tidyverse)

# --------------------------
# Prepare plotting data


fig5b_df <- vir %>%
  select(
    Isolate,
    GI_VG_Total
  ) %>%
  arrange(desc(GI_VG_Total))

fig5b_df$Isolate <- factor(
  fig5b_df$Isolate,
  levels = fig5b_df$Isolate
)

# --------------------------
# Create figure


fig5b <- ggplot(
  fig5b_df,
  aes(
    x = Isolate,
    y = GI_VG_Total
  )
) +
  
  geom_col(
    fill = "#d95f02",
    width = 0.8
  ) +
#add median line
  geom_hline(
    yintercept = median(fig5b_df$GI_VG_Total),
    linetype = "dashed",
    linewidth = 0.8,
    colour = "black"
  ) +
#annotate it
  annotate(
    "text",
    x = length(unique(fig5b_df$Isolate)) - 1,
    y = median(fig5b_df$GI_VG_Total) + 1,
    label = paste0(
      "Median = ",
      median(fig5b_df$GI_VG_Total)
    ),
    size = 4,
    fontface = "italic"
  ) +
  
  geom_text(
    aes(
      label = GI_VG_Total
    ),
    vjust = -0.4,
    fontface = "bold",
    size = 4
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.08)
    )
  ) +
  
  labs(
    x = NULL,
    y = "GI-associated virulence genes"
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank()
    
  )

# --------------------------
# Display


print(fig5b)

# --------------------------
# Export


ggsave(
  "Figure5B_GI_VirulenceGenes.png",
  plot = fig5b,
  width = 7,
  height = 5,
  dpi = 600
)

# ==========================================================
# End Figure 5B
