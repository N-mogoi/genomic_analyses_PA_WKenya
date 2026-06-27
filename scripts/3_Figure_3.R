

# ==========================================================
# Figure 3A
# Multidrug resistance burden per isolate


# --------------------------
# Load packages

library(tidyverse)

# ----------------------
# Read categorical dataset

cat_df <- read.csv(
  "inhibition_diameters_categorical.csv",
  stringsAsFactors = FALSE
)

# --------------------------
# Calculate MDR score

burden_df <- cat_df %>%
  mutate(
    MDR_score = apply(
      select(., -Isolate),
      1,
      function(x) sum(x == "R")
    )
  )

# --------------------------
# Define meropenem status

burden_df <- burden_df %>%
  mutate(
    MRP_group = ifelse(
      MRP == "R",
      "Meropenem-resistant",
      "Meropenem non-resistant"
    )
  )

# ----------------------
# Order isolates by MDR score

burden_df <- burden_df %>%
  arrange(desc(MDR_score)) %>%
  mutate(
    Isolate = factor(
      Isolate,
      levels = Isolate
    )
  )

# --------------------------
# Create figure
# -------------------
fig3a <- ggplot(
  burden_df,
  aes(
    x = Isolate,
    y = MDR_score,
    fill = MRP_group
  )
) +
  
  geom_col(
    width = 0.8,
    colour = "black"
  ) +
  
  geom_text(
    aes(label = MDR_score),
    vjust = -0.4,
    size = 4
  ) +
  
  scale_fill_manual(
    values = c(
      "Meropenem-resistant" = "#1b9e77",
      "Meropenem non-resistant" = "#d95f02"
    )
  ) +
  
  
  scale_y_continuous(
    limits = c(0, 7),
    breaks = 0:6
  ) +
  
  labs(
    x = "Isolate",
    y = "MDR score",
    fill = NULL
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    legend.position = "top"
  )

# --------------------------
# Display
# --------------
print(fig3a)

# --------------------------
# Export
# --------------------------
ggsave(
  "Figure3A_MDR_Burden.png",
  plot = fig3a,
  width = 7,
  height = 5,
  dpi = 600
)

# ==========================================
# End Figure 3A
=