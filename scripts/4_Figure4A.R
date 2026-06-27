

# ==========================================================
# Figure 4A
# Genome size vs genomic island abundance
# Bubble size = total GI length
# Bubble colour = % genome occupied by GIs
# Shape = plasmid presence


# --------------------------
# Load packages
# ------------------
library(tidyverse)
library(scales)

# --------------------------
# Read data

ann <- read.csv(
  "annotation_summary.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# --------------------------
# Clean columns


ann$`Chromosome Length (bp)` <- as.numeric(
  gsub(",", "", ann$`Chromosome Length (bp)`)
)

ann$`Genomic islands (GIs)` <- as.numeric(
  ann$`Genomic islands (GIs)`
)

ann$`Total length of GIs` <- as.numeric(
  gsub(",", "", ann$`Total length of GIs`)
)

ann$`% length GIs` <- as.numeric(
  ann$`% length GIs`
)

# --------------------------
# Create plotting variables


ann <- ann %>%
  mutate(
    
    Genome_Mb =
      `Chromosome Length (bp)` / 1e6,
    
    GI_Count =
      `Genomic islands (GIs)`,
    
    GI_Length_kb =
      `Total length of GIs` / 1000,
    
    GI_Percent =
      `% length GIs`,
    
    Plasmid =
      ifelse(
        `Plasmid length` %in% c("No", "NO", "", NA),
        "Absent",
        "Present"
      )
    
  )

# --------------------------
# Create figure


fig4a <- ggplot(
  ann,
  aes(
    x = Genome_Mb,
    y = GI_Count
  )
) +
  
  geom_point(
    aes(
      size = GI_Length_kb,
      colour = GI_Percent,
      shape = Plasmid
    ),
    alpha = 0.85,
    stroke = 1
  ) +
  
  geom_text(
    aes(
      label = `Isolate identity number`
    ),
    vjust = -1.0,
    size = 3.5
  ) +
  
  scale_size_continuous(
    name = "Total GI length (kb)",
    
    range = c(4, 12)
  ) +
  
  scale_colour_gradient(
    low = "#fdd0a2",
    high = "#d94801",
    name = "GI content (%)"
  ) +
  
  scale_shape_manual(
    values = c(
      "Absent" = 16,
      "Present" = 17
    ),
    name = "Plasmid"
  ) +
  
  labs(
    x = "Genome size (Mb)",
    y = "Number of genomic islands"
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    panel.grid.minor = element_blank(),
    
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold",
      size = 12
    ),
    
    legend.text = element_text(size = 11)
  )

# --------------------------
# Display figure


fig4a

# --------------------------
# Export figure


ggsave(
  "Figure4A_GenomeArchitecture.png",
  fig4a,
  width = 8,
  height = 6,
  dpi = 600
)

# ==================================================
# End Figure 4A
# =========================================