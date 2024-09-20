library(readxl)
library(tidyverse)
library(writexl)


# ------------------------------------------------------------------------------
# Import transactions

# Import 2023
pb_transactions <- read_xlsx("data/pb-exports/transactions - 2023.xlsx") |> 
  select(c(1, 2, 3, 4, 7, 8)) |> 
  janitor::clean_names() |> 
  mutate(
    date = as.Date(date),
    type = as.factor(type), # c("INVESTMENT", "BUYBACK_INTEREST", "BUYBACK_PRINCIPAL", "BUYBACK_PRINCIPAL", "BUYBACK_INTEREST", "DEPOSIT")
    loan_status = as.factor(loan_status)
  )

# Import 2024
pb_transactions <- read_xlsx("data/pb-exports/transactions - 2024.xlsx") |> 
  select(c(1, 2, 3, 4, 7, 8)) |> 
  janitor::clean_names() |> 
  mutate(
    date = as.Date(date),
    type = as.factor(type), # c("INVESTMENT", "BUYBACK_INTEREST", "BUYBACK_PRINCIPAL", "BUYBACK_PRINCIPAL", "BUYBACK_INTEREST", "DEPOSIT")
    loan_status = as.factor(loan_status)
  ) |> 
  rbind(pb_transactions)
