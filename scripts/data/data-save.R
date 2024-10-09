library(tidyverse)


ensure_dir(pb.data_dir)

# Save investments
write_rds(pb, file.path(pb.data_dir, "pb.rds"))

# Save transactions
write_rds(pb_transactions, file.path(pb.data_dir, "pb_transactions.rds"))
