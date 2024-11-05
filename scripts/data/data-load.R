library(tidyverse)

# ------------------------------------------------------------------------------
# Load investments

pb <- read_rds(file.path(config.data_dir, "pb.rds"))


# ------------------------------------------------------------------------------
# Load transactions

pb_transactions <- read_rds(file.path(config.data_dir, "pb_transactions.rds"))
