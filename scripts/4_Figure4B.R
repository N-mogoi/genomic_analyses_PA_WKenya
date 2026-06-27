# ==========================================================
# Figure 4B
# Ranked genomic island content
# ====================================================

library(tidyverse)

# --------------------------
# Read annotation summary

ann <- read.csv(
  "annotation_summary.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# --------------------------
# Clean GI percentage column
-
ann$`% length GIs` <- as.numeric(
  gsub(",", "", ann$`% length GIs`)
)

# --------------------------
# Prepare plotting data

plot_df <- ann %>%
  transmute(
    Isolate = `Isolate identity number`,
    GI_Content = `% length GIs`
  ) %>%
  arrange(desc(GI_Content))

plot_df$Isolate <- factor(
  plot_df$Isolate,
  levels = rev(plot_df$Isolate)
)

# --------------------------
# Create figure

fig4b <- ggplot(
  plot_df,
  aes(
    x = Isolate,
    y = GI_Content
  )
) +
  
  geom_col(
    fill = "#d95f02",
    width = 0.8
  ) +
  
  geom_text(
    aes(
      label = paste0(
        round(GI_Content, 1),
        "%"
      )
    ),
    hjust = -0.1,
    size = 4.5,
    fontface = "bold"
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    limits = c(
      0,
      max(plot_df$GI_Content) + 2
    ),
    breaks = seq(0, 12, 1),
    expand = c(0, 0)
  ) +
  
  labs(
    x = NULL,
    y = "GI content (% of genome)"
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    
    panel.grid.major.y = element_blank(),
    
    panel.grid.minor = element_blank(),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text.y = element_text(
      face = "bold"
    )
    
  )

# --------------------------
# Display

fig4b

# --------------------------
# Export

ggsave(
  "Figure4B_GI_Content.png",
  fig4b,
  width = 7,
  height = 6,
  dpi = 600
)

# ========================================================
# End Figure 4B
# =================================================