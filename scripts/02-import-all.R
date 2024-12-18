# ------------------------------------------------------------------------------
# Import from excel files

source("scripts/import-export/import-loans-excel.R")
source("scripts/import-export/import-transactions-excel.R")


# ------------------------------------------------------------------------------
# Calculate important dates

pb_transactions.first_date <- min(pb_transactions$date)
pb_transactions.last_date <- max(pb_transactions$date)

pb_transactions.first_month_as_date <- floor_date(pb_transactions.first_date, "month")
pb_transactions.last_month_as_date <- floor_date(pb_transactions.last_date, "month")

pb_transactions.first_year_as_date <- floor_date(pb_transactions.first_date, "year")
pb_transactions.first_year <- year(pb_transactions.first_year_as_date)

pb_transactions.last_year_as_date <- floor_date(pb_transactions.last_date, "year")
pb_transactions.last_year <- year(pb_transactions.last_year_as_date)

pb_transactions.current_date = max(pb_transactions$date)

pb_transactions.current_month = month(pb_transactions.current_date)
pb_transactions.current_month_as_date = floor_date(pb_transactions.current_date, "month")

pb_transactions.current_year = year(pb_transactions.current_date)
pb_transactions.current_year_as_date = floor_date(pb_transactions.current_date, "year")

pb_today <- today()
#pb_today <- pb_transactions.last_date
#pb_today <- ymd("2024-11-18")


# ------------------------------------------------------------------------------
# Calculate countries

pb_country_levels <- pb_loans |> 
  group_by(country) |> 
  summarize()

pb_country_levels <- pb_country_levels[['country']]


# ------------------------------------------------------------------------------
# Late Investments

pb.late_levels <- c("in time", "late 1-15", "late 16-30", "late 31-60", "defaulted")

pb.late_category <- function(days) {
  fct(
    case_when(
      days >= 0 ~ "in time",
      days >= -15 ~ "late 1-15",
      days >= -30 ~ "late 16-30",
      days >= -60 ~ "late 31-60",
      .default = "defaulted"
    ),
    levels = pb.late_levels
  )
}
