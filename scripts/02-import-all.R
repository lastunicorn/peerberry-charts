# ------------------------------------------------------------------------------
# Import from excel files

source("scripts/import-export/import-loans-excel.R")
source("scripts/import-export/import-transactions-excel.R")


# ------------------------------------------------------------------------------
# Calculate important values

pb_transactions.first_date <- min(pb_transactions$date)
pb_transactions.last_date <- max(pb_transactions$date)

pb_transactions.first_month_as_date <- floor_date(pb_transactions.first_date, "month")
pb_transactions.last_month_as_date <- floor_date(pb_transactions.last_date, "month")

pb_transactions.first_year_as_date <- floor_date(pb_transactions.first_date, "year")
pb_transactions.last_year_as_date <- floor_date(pb_transactions.last_date, "year")

pb_transactions.current_date = max(pb_transactions$date)

pb_transactions.current_month = month(pb_transactions.current_date)
pb_transactions.current_month_as_date = floor_date(pb_transactions.current_date, "month")

pb_transactions.current_year = year(pb_transactions.current_date)
pb_transactions.current_year_as_date = floor_date(pb_transactions.current_date, "year")
