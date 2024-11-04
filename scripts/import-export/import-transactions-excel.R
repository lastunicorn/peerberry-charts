library(readxl)


# ------------------------------------------------------------------------------
# Import transactions

pb_transactions <- read_and_merge_excels(
  file.path(pb.import_dir, list.files(pb.import_dir, pattern = "transactions - \\d+\\.xlsx"))
) |> 
  #select(c(1, 2, 3, 4, 6, 7, 8)) |>
  janitor::clean_names() |>
  mutate(
    date = as.Date(date),
    amount = as.numeric(amount)
  ) |> 
  convert_to_factor(type, c("INVESTMENT", "BUYBACK_INTEREST", "BUYBACK_PRINCIPAL", "DEPOSIT", "REPAYMENT_INTEREST", "REPAYMENT_PRINCIPAL")) |> 
  convert_to_factor(loan_status, c("CURRENT", "LATE", "FINISHED", NA))


# ------------------------------------------------------------------------------
# Calculate important values

pb_transactions.first_date <- min(pb_transactions$date)
pb_transactions.last_date <- max(pb_transactions$date)

pb_transactions.first_month_as_date <- floor_date(pb_transactions.first_date, "month")
pb_transactions.last_month_as_date <- floor_date(pb_transactions.last_date, "month")

pb_transactions.first_year_as_date <- floor_date(pb_transactions.first_date, "year")
pb_transactions.last_year_as_date <- floor_date(pb_transactions.last_date, "year")