# ------------------------------------------------------------------------------
# ROI - Net annualized return % by month - per year

pb_transactions |> 
  group_by(date) |> 
  summarize(
    interest = sum(if_else(type %in% c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"), amount, 0)),
    funds_delta = sum(funds_delta)
  ) |> 
  mutate(
    funds = cumsum(funds_delta)
  ) |> 
  arrange(date) |> 
  mutate(
    start_date = min(date),
    end_date = max(date) + days(1),
    month_as_date = floor_date(date, "month")
  ) |> 
  add_row(
    month_as_date = generate_monthly_dates(pb_transactions.first_year_as_date, pb_transactions.last_year_as_date + months(11))
  ) |>
  mutate(
    roi_percentage = if_else(is.na(lag(funds)), 0, interest * 100 / lag(funds)),
    investment_days = if_else(is.na(lag(date)), 0, as.numeric(date - lag(date))),
    roi_percentage_pa = if_else(investment_days == 0, 0, roi_percentage * 365 / investment_days)
  ) |> 
  group_by(month_as_date) |> 
  mutate(
    month_first_date = floor_date(date, "month"),
    month_first_date = if_else(month_first_date < pb_transactions.first_date, pb_transactions.first_date, month_first_date),
    month_last_date = ceiling_date(date, "month") - days(1),
    month_last_date = if_else(month_last_date > pb_transactions.last_date, pb_transactions.last_date, month_last_date),
    month_interval = month_first_date %--% (month_last_date + days(1)),
    days_in_month = month_interval / days(1)
  ) |> 
  summarize(
    roi = sum(roi_percentage_pa * investment_days / first(days_in_month))
  ) |> 
  mutate(
    year = as.factor(year(month_as_date))
  ) |> 
  ggplot(aes(x = month_as_date, y = roi)) +
  geom_col() +
  geom_text(
    aes(label = paste(format(round(roi, 2), nsmall = 2), "%")),
    vjust = -0.5,
    size = 3
  ) +
  facet_wrap(~ year, ncol = 1, scales = "free_x") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    minor_breaks = NULL
  ) +
  labs(
    title = "Net annualized return % (including cash drag) by month - per year",
    x = "Month",
    y = "ROI (% p.a.)"
  )

save_plot("roi-03-by-month-per-year.png")
