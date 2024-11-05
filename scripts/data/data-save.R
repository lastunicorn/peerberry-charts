library(tidyverse)


ensure_dir(config.data_dir)

# Save investments
write_rds(pb, file.path(config.data_dir, "pb.rds"))

# Save transactions
write_rds(pb_transactions, file.path(config.data_dir, "pb_transactions.rds"))
