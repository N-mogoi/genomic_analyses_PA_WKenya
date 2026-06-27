
# ==========================================================
# Figure 4
# Combined genome architecture figure
# Panel A: Bubble plot
# Panel B: Ranked GI content
# 

#--------------------------------------------
getwd()

list.files()

#load required libraries
library(patchwork)

fig4a <- fig4a +
  labs(tag = "A") +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))
  
  
fig4b <- fig4b +
  labs(tag = "B") +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))
  
  
Figure4 <- fig4a + fig4b +
  plot_layout(widths = c(1.5, 1)) +
  
  theme(plot.tag = element_text(
    face = "bold",
    size = 18
  )
  )

plot.tag.position = c(0.02, 0.98)

#display figure
Figure4

ggsave("Figure4_GenomeArchitecture.png",Figure4,
       width = 14,
       height = 6,
       dpi = 600)

#-----------------------------------------------------------------

#END