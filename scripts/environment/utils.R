# ------------------------------------------------------------------------------
# read_and_merge_excels

# Used to import data from excel files.
# Reads multiple excel files with same structure and merge them into a single
# tibble.

read_and_merge_excels <- function(file_list) {
  # Use lapply to read each file with specified column types and store the results in a list of tibbles
  tibbles_list <- lapply(file_list, function(file) {
    read_excel(file, col_types = c("text"))
  })
  
  # Combine all tibbles into a single tibble using bind_rows
  merged_tibble <- bind_rows(tibbles_list)
  
  return(merged_tibble)
}


# ------------------------------------------------------------------------------
# convert_to_factor

# Used at import time to change the type of a colulmn into a factor.
# An additional check is performed to ensure the column contains only allowed
# values.

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
# interest_percentage_peranum

interest_percentage_peranum <- function(start_date, end_date, start_funds, interest) {
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)
  
  roi_percentage = if_else(is.na(start_funds), 0, interest * 100 / start_funds)
  investment_days = if_else(is.na(start_date), 0, as.numeric(end_date - start_date))
  roi_percentage_pa = if_else(investment_days == 0, 0, roi_percentage * 365 / investment_days)
  
  return(roi_percentage_pa)
}


# ------------------------------------------------------------------------------
# pb.days_in_month

pb.days_in_month <- function(date) {
  date = as.Date(date)
  
  month_first_date = floor_date(date, "month")
  month_first_date = if_else(month_first_date < pb_transactions.first_date, pb_transactions.first_date, month_first_date)
  
  month_last_date = ceiling_date(date, "month") - days(1)
  month_last_date = if_else(month_last_date > pb_transactions.last_date, pb_transactions.last_date, month_last_date)
  
  month_interval = month_first_date %--% (month_last_date + days(1))
  days_in_month = month_interval / days(1)
  
  return(days_in_month)
}

# ------------------------------------------------------------------------------
# format_decimal

format_decimal <- function(x, n = 2) {
  as.numeric(x) |>
    round(n) |>
    format(nsmall = n, big.mark=",")
}
