# ==========================================================
# Quorum sensing (QS) data harmonisation
# Creates:
#   1. QS_raw.csv
#   2. QS_outputs.csv
# ===================================================

# --------------------------
# Load packages


library(tidyverse)

# --------------------------
# Working directory

getwd()

setwd("/Users/nickm/Documents/Projects/Kindiki/QS_Raw_data")

# --------------------------
# Read raw biosensor datasets


bhl <- read.csv(
  "BHL_raw.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

oddhl <- read.csv(
  "OdDHL_raw.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(oddhl)

pqs <- read.csv(
  "PQS_raw.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# -----------------------
# Standardise isolate ID column

# Remove accidental spaces

bhl$IsolateID   <- trimws(bhl$IsolateID)
oddhl$IsolateID <- trimws(oddhl$IsolateID)
pqs$IsolateID   <- trimws(pqs$IsolateID)

# --------------------------
# Add signal labels

bhl$Signal   <- "BHL"
oddhl$Signal <- "OdDHL"
pqs$Signal   <- "PQS"

# --------------------------
# Convert to long format


bhl_long <- bhl %>%
  pivot_longer(
    cols = Rep1:Rep3,
    names_to = "Replicate",
    values_to = "Luminescence"
  )

oddhl_long <- oddhl %>%
  pivot_longer(
    cols = Rep1:Rep3,
    names_to = "Replicate",
    values_to = "Luminescence"
  )

pqs_long <- pqs %>%
  pivot_longer(
    cols = Rep1:Rep3,
    names_to = "Replicate",
    values_to = "Luminescence"
  )

# --------------------------
# Merge datasets
# -------------------

qs_raw <- bind_rows(
  bhl_long,
  oddhl_long,
  pqs_long
)

# --------------------------
# Clean variables
# ----------------------

qs_raw <- qs_raw %>%
  mutate(
    Luminescence = as.numeric(Luminescence),
    Signal = factor(
      Signal,
      levels = c("BHL", "OdDHL", "PQS")
    ),
    Replicate = factor(
      Replicate,
      levels = c("Rep1", "Rep2", "Rep3")
    )
  ) %>%
  arrange(
    Signal,
    IsolateID,
    Replicate
  )

# --------------------------
# Keep only required columns
# -------------------

qs_raw <- qs_raw %>%
  select(
    IsolateID,
    Signal,
    Replicate,
    Luminescence
  )

# --------------------------
# Export raw dataset
# ----------------------

write.csv(
  qs_raw,
  "QS_raw.csv",
  row.names = FALSE
)

# ==========================================================
# Summary statistics
# ============================================

qs_outputs <- qs_raw %>%
  group_by(
    IsolateID,
    Signal
  ) %>%
  summarise(
    Mean = mean(Luminescence),
    SD = sd(Luminescence),
    SEM = SD / sqrt(n()),
    N = n(),
    Minimum = min(Luminescence),
    Maximum = max(Luminescence),
    .groups = "drop"
  ) %>%
  arrange(
    Signal,
    IsolateID
  )

# --------------------------
# Export summary statistics
# ----------------------

write.csv(
  qs_outputs,
  "QS_outputs.csv",
  row.names = FALSE
)

# --------------------------
# Check outputs
# ----------------------

names(qs_raw)

head(qs_raw)

head(qs_outputs)

nrow(qs_raw)

table(qs_raw$Signal)

length(unique(qs_raw$IsolateID))

# =======================================================
# End
# ================================================