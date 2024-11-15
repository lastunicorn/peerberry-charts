library(readxl)


# ------------------------------------------------------------------------------
# Import transactions

pb_transactions <- read_and_merge_excels(
  file.path(config.import_dir, list.files(config.import_dir, pattern = "transactions - \\d+\\.xlsx"))
) |> 
  #select(c(1, 2, 3, 4, 6, 7, 8)) |>
  janitor::clean_names() |>
  mutate(
    date = as.Date(date),
    amount = as.numeric(amount)
  ) |> 
  convert_to_factor(type, c("INVESTMENT", "BUYBACK_INTEREST", "BUYBACK_PRINCIPAL", "DEPOSIT", "REPAYMENT_INTEREST", "REPAYMENT_PRINCIPAL")) |> 
  convert_to_factor(loan_status, c("CURRENT", "LATE", "FINISHED", "WAITING_BUYBACK", NA)) |> 
  mutate(
    funds_delta = case_when(
      type %in% c("WITHDRAW") ~ -amount,
      type %in% c("DEPOSIT", "BUYBACK_INTEREST", "REPAYMENT_INTEREST") ~ amount,
      .default = 0
    )
  )