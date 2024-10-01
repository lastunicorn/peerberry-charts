library(readxl)
library(tidyverse)


# ------------------------------------------------------------------------------
# Import transactions


# # Import 2023
# pb_transactions <- read_xlsx("data-raw/transactions - 2023.xlsx") |> 
#   select(c(1, 2, 3, 4, 7, 8)) |> 
#   janitor::clean_names() |> 
#   mutate(
#     date = as.Date(date),
#     type = as.factor(type), 
#     loan_status = as.factor(loan_status)
#   )
# 
# # Import 2024
# pb_transactions <- read_xlsx("data-raw/transactions - 2024.xlsx") |> 
#   select(c(1, 2, 3, 4, 7, 8)) |> 
#   janitor::clean_names() |> 
#   mutate(
#     date = as.Date(date),
#     type = as.factor(type),
#     loan_status = as.factor(loan_status)
#   ) |> 
#   rbind(pb_transactions)


# ------------------------------------------------------------------------------
# Functions

read_and_merge_excels <- function(file_list) {
  # Use lapply to read each file with specified column types and store the results in a list of tibbles
  tibbles_list <- lapply(file_list, function(file) {
    read_excel(file, col_types = c("text"))
  })
  
  # Combine all tibbles into a single tibble using bind_rows
  merged_tibble <- bind_rows(tibbles_list)
  
  return(merged_tibble)
}


convert_to_factor <- function(data, column, allowed_levels) {
  
  # Extract the column as a vector
  column_data <- pull(data, {{column}})
  
  # Find any values in the column that are not in the allowed levels
  invalid_values <- setdiff(unique(column_data), allowed_levels)
  
  # If there are invalid values, stop and throw an error
  if (length(invalid_values) > 0) {
    stop(paste("Invalid values found in", deparse(substitute(column)), ":", paste(invalid_values, collapse = ", ")))
  }
  
  # If no invalid values, convert the column to a factor with the allowed levels
  data <- data %>%
    mutate({{column}} := factor({{column}}, levels = allowed_levels))
  
  return(data)
}


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

