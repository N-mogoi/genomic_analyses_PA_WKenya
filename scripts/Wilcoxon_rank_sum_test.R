#Figure 3; Wilcoxon rank-sum test

library(tidyverse)

cat_df <- read.csv(
  "inhibition_diameters_categorical.csv",
  stringsAsFactors = FALSE
)

burden_df <- cat_df %>%
  mutate(
    MDR_score = apply(
      select(., -Isolate),
      1,
      function(x) sum(x == "R")
    )
  )

burden_df <- burden_df %>%
  mutate(
    MRP_group = ifelse(
      MRP == "R",
      "Meropenem-resistant",
      "Meropenem non-resistant"
    )
  )

t.test(
  MDR_score ~ MRP_group,
  data = burden_df
)

wilcox.test(
  MDR_score ~ MRP_group,
  data = burden_df,
  exact = FALSE
)

burden_df[, c("Isolate","MDR_score","MRP","MRP_group")]