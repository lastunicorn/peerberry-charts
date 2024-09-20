library(tidyverse)

# ------------------------------------------------------------------------------
# Load investments

pb <- read_rds("data/pb.rds")


# ------------------------------------------------------------------------------
# Load transactions

pb_transactions <- read_rds("data/pb_transactions.rds")
