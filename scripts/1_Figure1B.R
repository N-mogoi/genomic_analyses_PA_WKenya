
# ==========================================================
# Figure 1B
# Resistance prevalence with 95% confidence intervals
# ==============================================

library(tidyverse)

getwd()
setwd("/Users/nickm/Documents/Projects/Kindiki")
# --------------------------
# Read categorical data
# -----------
cat_df <- read.csv(
  "inhibition_diameters_categorical.csv",
  stringsAsFactors = FALSE
)

# --------------------------
# Calculate resistance
# prevalence per antibiotic


res_summary <- map_dfr(
  names(cat_df)[-1],
  function(ab){
    
    n_res <- sum(cat_df[[ab]] == "R")
    n_tot <- nrow(cat_df)
    
    ci <- binom.test(n_res, n_tot)$conf.int
    
    data.frame(
      Antibiotic = ab,
      Resistant = n_res,
      Total = n_tot,
      Percent = 100 * n_res / n_tot,
      Lower = 100 * ci[1],
      Upper = 100 * ci[2]
    )
    
  }
)

# --------------------------
# Order by prevalance highest to lowest


res_summary <- res_summary %>%
  arrange(desc(Percent)) %>%
  mutate(
    Antibiotic = factor(Antibiotic, levels = Antibiotic))


#------------------------------------------------
#plot

fig1b <- ggplot(
  res_summary,
  aes(
    x = Antibiotic,
    y = Percent
  )
) +
  
  # Bars
  geom_col(
    fill = "#d95f02",
    width = 0.7
  ) +
  
  # Error bars
  geom_errorbar(
    aes(
      ymin = Lower,
      ymax = Upper
    ),
    width = 0.15,
    height = 0.2,
    linewidth = 0.35
  ) +
  
  # Percentage labels ABOVE the CI
  # Bold percentage
  geom_text(
    aes(
      y = Upper + 6,
      label = paste0(round(Percent), "%")
    ),
    size = 4.5,
    fontface = "bold"
  ) +
  
  # Normal count underneath
  geom_text(
    aes(
      y = Upper + 2.5,
      label = paste0("(", Resistant, "/", Total, ")")
    ),
    size = 4,
    fontface = "plain"
  ) +
  
  # Y-axis
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    expand = expansion(mult = c(0, 0.07))
  ) +
  
  labs(
    x = NULL,
    y = "Resistance prevalence (%)"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 10
    ),
    
    axis.text.y = element_text(
      size = 14
    ),
    
    axis.title.y = element_text(
      size = 18,
    ),
    
    axis.line = element_line(
      linewidth = 0.6
    ),
    
    axis.ticks = element_line(
      linewidth = 0.6
    )
  )


# Display


fig1b

# --------------------------
# Export


ggsave(
  "Figure1B_ResistancePrevalence.png",
  fig1b,
  width = 8,
  height = 6,
  dpi = 600
)



#nd of 1B
