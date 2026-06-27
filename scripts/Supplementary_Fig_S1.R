

# ==========================================================
# AMR Analysis
# Association between accessory AMR genes
# and QS regulator genotype
# ==========================================================

# --------------------------
# Load packages
# --------------------------

library(tidyverse)
library(rstatix)
library(broom)

# --------------------------
# Read data
# --------------------------

amr <- read.csv(
  "AMR_genes_S1.csv",
  skip = 6,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

qs_meta <- read.csv(
  "QS_metadata.csv",
  stringsAsFactors = FALSE
)

# --------------------------
# Extract column names
# --------------------------

new_names <- as.character(amr[1, ])

new_names[1] <- "Gene"
new_names[length(new_names)-1] <- "Product"
new_names[length(new_names)] <- "Resistance"

colnames(amr) <- new_names

amr <- amr[-1, ]

# remove empty columns

amr <- amr[, colnames(amr) != ""]
amr <- amr[, !is.na(colnames(amr))]

# --------------------------
# Clinical isolates only
# --------------------------

iso_cols <- c(
  "238627E_ID003",
  "238628E_ID004",
  "238629E_ID006",
  "238630E_ID008",
  "238631E_ID009",
  "238632E_ID011",
  "238633E_ID012",
  "238634E_ID013",
  "238635E_ID014",
  "238636E_ID015",
  "238637E_ID016",
  "238638E_ID017",
  "238641E_ID020"
)

# --------------------------
# Variable accessory genes
# --------------------------

selected_genes <- c(
  
  "APH(3')-Ia",
  
  "APH(6)-Id",
  
  "APH(3'')-Ib",
  
  "CrpP",
  
  "PDC-10",
  
  "PmrB"
  
)

amr_sub <- amr %>%
  filter(
    Gene %in% selected_genes
  ) %>%
  select(
    Gene,
    all_of(iso_cols)
  )

# --------------------------
# Convert to presence/absence
# --------------------------

for(i in 2:ncol(amr_sub)){
  
  amr_sub[[i]] <- case_when(
    
    amr_sub[[i]] %in% c("-", ".") ~ 0,
    
    TRUE ~ 1
    
  )
  
}

# --------------------------
# Transpose
# --------------------------

amr_mat <- t(
  as.matrix(
    amr_sub[, -1]
  )
)

colnames(amr_mat) <- amr_sub$Gene

rownames(amr_mat) <- c(
  
  "ID003",
  
  "ID004",
  
  "ID006",
  
  "ID008",
  
  "ID009",
  
  "ID011",
  
  "ID012",
  
  "ID013",
  
  "ID014",
  
  "ID015",
  
  "ID016",
  
  "ID017",
  
  "ID020"
  
)

amr_df <- as.data.frame(
  amr_mat
)

amr_df$IsolateID <- rownames(
  amr_df
)

# convert to numeric

amr_df <- amr_df %>%
  mutate(
    across(
      -IsolateID,
      as.numeric
    )
  )

# --------------------------
# Merge metadata
# --------------------------

dat <- left_join(
  
  amr_df,
  
  qs_meta,
  
  by = "IsolateID"
  
)

table(dat$QS_group)


# ==========================================================
# Fisher tests
# ==========================================================

results <- data.frame(
  
  Gene = character(),
  
  Odds_ratio = numeric(),
  
  Lower_CI = numeric(),
  
  Upper_CI = numeric(),
  
  P = numeric(),
  
  stringsAsFactors = FALSE
  
)

#let script determine which genes are available
genes_to_test <- intersect(
  selected_genes,
  names(dat)
)

genes_to_test

#loop
for(g in genes_to_test){
  
  cat("\nTesting:", g, "\n")
  
  if(!(g %in% names(dat))){
    
    cat("Gene not found in data frame.\n")
    
    next
    
  }
  
  tab <- table(
    
    dat[[g]],
    
    dat$QS_group
    
  )
  
  cat("\n====================\n")
  cat(g, "\n")
  print(tab)
  
  names(amr_df)
  # Skip genes that cannot produce a 2 x 2 table
  if(!all(dim(tab) == c(2,2))){
    
    results <- rbind(
      
      results,
      
      data.frame(
        
        Gene = g,
        
        Odds_ratio = NA,
        
        Lower_CI = NA,
        
        Upper_CI = NA,
        
        P = NA,
        
        stringsAsFactors = FALSE
        
      )
      
    )
    
    next
    
  }
  
  ft <- fisher.test(
    
    tab,
    
    conf.int = TRUE
    
  )
  
  results <- rbind(
    
    results,
    
    data.frame(
      
      Gene = g,
      
      Odds_ratio = unname(ft$estimate),
      
      Lower_CI = ft$conf.int[1],
      
      Upper_CI = ft$conf.int[2],
      
      P = ft$p.value,
      
      stringsAsFactors = FALSE
      
    )
    
  )
  
}

# --------------------------
# Multiple testing correction
# --------------------------

results$P_adj <- p.adjust(
  
  results$P,
  
  method = "BH"
  
)

results <- results %>%
  
  arrange(P_adj)

print(results)

# --------------------------
# Export
# --------------------------

write.csv(
  
  results,
  
  "AMR_Gene_Association_Fisher.csv",
  
  row.names = FALSE
  
)

# ==========================================================
# Forest plot
# ==========================================================

results_plot <- results %>%
  
  filter(
    
    !is.na(Odds_ratio)
    
  ) %>%
  
  mutate(
    
    Gene = factor(
      
      Gene,
      
      levels = rev(Gene)
      
    )
    
  )

p_amr <-
  
  ggplot(results_plot,
         aes(x = Gene,
             y = Odds_ratio)) +
  
  geom_linerange(
    aes(
      ymin = Lower_CI,
      ymax = Upper_CI
    ),
    linewidth = 0.5,
    colour = "#1b9e77"
  ) +
  
  geom_point(
    size = 3,
    colour = "#1b9e77"
  ) +
  
  geom_hline(
    yintercept = 1,
    linetype = 2,
    colour = "grey50"
  ) +
  
  coord_flip() +
  
  scale_y_log10() +
  
  theme_classic(base_size = 10) +
  
  labs(
    x = "",
    y = "Odds ratio (95% CI)"
  )

p_amr

ggsave(
  
  "Supplementary_Figure_AMR_Fisher_OR.png",
  
  p_amr,
  
  width = 6,
  
  height = 4,
  
  dpi = 900
  
)

