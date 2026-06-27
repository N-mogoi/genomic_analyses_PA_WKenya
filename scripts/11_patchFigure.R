# ==========================================================
# Figure 11
# Patch together panels A, B and C
# =====================================================

library(patchwork)

# ----------------------------------------------------------
# Standardise theme across panels
# ---------------------------------------------

common_theme <- theme(
  
  text = element_text(size = 10),
  
  axis.title = element_text(
    face = "bold",
    size = 10
  ),
  
  axis.text = element_text(
    size = 9
  ),
  
  legend.title = element_text(
    face = "bold",
    size = 9
  ),
  
  legend.text = element_text(
    size = 8
  ),
  
  strip.text = element_text(
    face = "bold",
    size = 10
  )
  
)

p  <- p  + common_theme
p_1 <- p_1 + common_theme
p_2 <- p_2 + common_theme

# ----------------------------------------------------------
# Layout
# A spans full width
# B and C below
# --------------------------------------------------

figure11 <-
  
  p /
  
  (p_1 | p_2) +
  
  plot_annotation(
    
    tag_levels = "A",
    
    theme = theme(
      
      plot.tag = element_text(
        
        face = "bold",
        
        size = 18
        
      )
      
    )
    
  )

# ----------------------------------------------------------
# Draw
# --------------------------------------------------------

figure11

# ----------------------------------------------------------
# Save
# -----------------------------------------------------

ggsave(
  
  filename = "Figure11_QS.pdf",
  
  plot = figure11,
  
  width = 11,
  
  height = 10,
  
  dpi = 600,
  
  bg = "white"
  
)

ggsave(
  
  filename = "Figure11_QS.png",
  
  plot = figure11,
  
  width = 11,
  
  height = 10,
  
  dpi = 600,
  
  bg = "white"
  
)

# =======================================================
# End Figure 11
# ==========================================================