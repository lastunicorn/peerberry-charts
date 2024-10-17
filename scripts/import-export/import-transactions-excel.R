library(readxl)
library(tidyverse)


# ------------------------------------------------------------------------------
# Import transactions

pb_transactions <- read_and_merge_excels(c(
  "data-raw/transactions - 2023.xlsx",
  "data-raw/transactions - 2024.xlsx"
)) |> 
  #select(c(1, 2, 3, 4, 6, 7, 8)) |>
  janitor::clean_names() |>
  mutate(
    date = as.Date(date),
    amount = as.numeric(amount)
  ) |> 
  convert_to_factor(type, c("INVESTMENT", "BUYBACK_INTEREST", "BUYBACK_PRINCIPAL", "DEPOSIT", "REPAYMENT_INTEREST", "REPAYMENT_PRINCIPAL")) |> 
  convert_to_factor(loan_status, c("CURRENT", "LATE", "FINISHED", NA))

