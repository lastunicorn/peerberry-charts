library(tidyverse)

# ------------------------------------------------------------------------------
# Save investments

write_rds(pb, "data/pb.rds")


# ------------------------------------------------------------------------------
# Save transactions

write_rds(pb_transactions, "data/pb_transactions.rds")
