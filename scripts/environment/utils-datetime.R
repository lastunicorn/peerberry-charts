# ------------------------------------------------------------------------------

generate_monthly_dates <- function(start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  dates <- seq(
    from = floor_date(start_date, "month"), 
    to = floor_date(end_date, "month"), 
    by = "month"
  )
  
  return(dates)
}


# ------------------------------------------------------------------------------

generate_weekly_dates <- function(start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  dates <- seq(
    from = floor_date(start_date, "week"), 
    to = floor_date(end_date, "week"), 
    by = "week"
  )
  
  return(dates)
}


# ------------------------------------------------------------------------------

last_month_of_year <- function (date) {
  date <- as.Date(date)
  
  month_as_date <- floor_date(date, "month") + months(11)
  
  return (month_as_date)
}


# ------------------------------------------------------------------------------

last_week_of_year <- function (date) {
  date <- as.Date(date)
  
  week_as_date <- ceiling_date(date, "year") - days(1)
  week_as_date <- floor_date(week_as_date, "week")
  
  return (week_as_date)
}


# ------------------------------------------------------------------------------

pb_transactions.add_start_end_dates <- function(data) {
  fake_rows <- pb_transactions |> 
    mutate(
      year = year(date)
    ) |>
    group_by(year) |> 
    reframe(
      date = c(make_date(min(year), 1, 1), make_date(max(year), 12, 31)),
      amount = c(0, 0)
    )
  
  return (bind_rows(data, fake_rows))
}


# ------------------------------------------------------------------------------

pb_transactions.add_year_start_end_dates <- function(data) {
  fake_rows <- pb_transactions |> 
    mutate(
      year = year(date)
    ) |>
    group_by(year) |> 
    reframe(
      date = c(make_date(unique(year), 1, 1), make_date(unique(year), 12, 31)),
      amount = c(0, 0)
    )
  
  return (bind_rows(data, fake_rows))
}


# ------------------------------------------------------------------------------

pb_transactions.days_in_month <- function(date) {
  date = as.Date(date)
  
  month_first_date = floor_date(date, "month");
  month_first_date = if_else(month_first_date < pb_transactions.first_date, pb_transactions.first_date, month_first_date);
  
  month_last_date = ceiling_date(date, "month") - days(1);
  month_last_date = if_else(month_last_date > pb_today, pb_today, month_last_date);
  
  month_interval = month_first_date %--% (month_last_date + days(1));
  days_in_month = month_interval / days(1);
  
  return(days_in_month);
}


# ------------------------------------------------------------------------------

pb_transactions.days_in_year <- function(year) {
  year = as.numeric(year)
  
  year_first_date = make_date(year, 1, 1)
  
  year_last_date = ceiling_date(year_first_date, "year") - days(1);
  year_last_date = if_else(year_last_date > pb_today, pb_today, year_last_date);
  
  year_interval = year_first_date %--% (year_last_date + days(1));
  days_in_year = year_interval / days(1);
  
  return(days_in_year);
}


# ------------------------------------------------------------------------------
# month_levels

month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
