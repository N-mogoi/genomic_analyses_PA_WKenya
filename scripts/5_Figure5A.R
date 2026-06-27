#---------------------------------------------------------
#Figure 5A
#Virulence genes; core chromosome, GI and plasmids

#------------------------------------

library(tidyverse)

#Read data
vir <- read.csv("virulence_summary.csv", stringsAsFactors = FALSE)

str(vir)

#Extract total GI-associated
# virulence genes
# --------------------------

vir <- vir %>%
  mutate(
    
    GI_VG_Total = as.numeric(
      sub(
        ".*TOTAL:",
        "",
        GI_VG
      )
    )
    
  )

# --------------------------
# Calculate total virulence
# gene content
# ------------------

vir <- vir %>%
  mutate(
    
    Total_VG =
      Core_VG +
      GI_VG_Total +
      Plasmid_VG
    
  )

#order isolates by total geen count
vir <- vir %>%
  mutate(Total_VG = Core_VG + GI_VG_Total + Plasmid_VG) %>%
  arrange(desc(GI_VG_Total))

vir$Isolate <- factor(vir$Isolate, levels = vir$Isolate)


##############
# --------------------------
# Convert to long format
# ----------------------

vir_long <- vir %>%
  select(
    Isolate,
    Core_VG,
    GI_VG_Total,
    Plasmid_VG
  ) %>%
  pivot_longer(
    cols = c(
      Core_VG,
      GI_VG_Total,
      Plasmid_VG
    ),
    names_to = "Location",
    values_to = "Count"
  )

# --------------------------
# Total virulence genes
# per isolate


vir_totals <- vir %>%
  select(
    Isolate,
    Total_VG
  )

# --------------------------
# Create figure


fig5a <- ggplot(
  vir_long,
  aes(
    x = Isolate,
    y = Count,
    fill = Location
  )
) +
  
  geom_col(
    width = 0.8,
    colour = "white"
  ) +
  
  # Total gene counts above bars
  geom_text(
    data = vir_totals,
    aes(
      x = Isolate,
      y = Total_VG,
      label = Total_VG
    ),
    inherit.aes = FALSE,
    vjust = -0.4,
    fontface = "bold",
    size = 4
  ) +
  
  scale_fill_manual(
    values = c(
      "Core_VG" = "#1f78b4",
      "GI_VG_Total" = "#d95f02",
      "Plasmid_VG" = "red"
    ),
    labels = c(
      "Core chromosome",
      "Genomic islands",
      "Plasmids"
    ),
    name = "Virulence gene location"
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.08)
    )
  ) +
  
  labs(
    x = NULL,
    y = "Number of virulence-associated genes"
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
    
    legend.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank()
  )

# --------------------------
# Display


fig5a

# --------------------------
# Export


ggsave(
  "Figure5A_VirulencePartitioning.png",
  plot = fig5a,
  width = 8,
  height = 5,
  dpi = 600
)



################
#End of 5A



























