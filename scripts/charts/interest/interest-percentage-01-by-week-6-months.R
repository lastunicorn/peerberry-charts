# ------------------------------------------------------------------------------
# ROI - Interest % by week - last 6 months

pb_transactions |> 
  group_by(date) |> 
  summarize(
    interest = sum(if_else(type %in% c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"), amount, 0)),
    funds_delta = sum(funds_delta)
  ) |> 
  mutate(
    funds = cumsum(funds_delta)
  ) |> 
  filter(date >= pb_transactions.current_month_as_date - months(6)) |> 
  arrange(date) |> 
  mutate(
    start_date = min(date),
    end_date = max(date) + days(1),
    week_as_date = floor_date(date, "week"),
    roi_percentage = if_else(is.na(lag(funds)), 0, interest * 100 / lag(funds)),
    investment_days = if_else(is.na(lag(date)), 0, as.numeric(date - lag(date))),
    roi_percentage_pa = if_else(investment_days == 0, 0, roi_percentage * 365 / investment_days)
  ) |> 
  group_by(week_as_date) |> 
  mutate(
    week_start_date = floor_date(date, "week"),
    week_start_date = if_else(week_start_date < start_date, start_date, week_start_date),
    week_end_date = ceiling_date(date, "week"),
    week_end_date = if_else(week_end_date > end_date, end_date, week_end_date),
    days_in_week = (week_start_date %--% week_end_date) / days(1)
  ) |> 
  summarize(
    roi = sum(roi_percentage_pa * investment_days / first(days_in_week))
  ) |> 
  ggplot(aes(x = week_as_date, y = roi)) +
  geom_col() +
  geom_text(
    aes(label = paste(format(round(roi, 2), nsmall = 2), "%")),
    vjust = -0.5
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    minor_breaks = NULL
  ) +
  labs(
    title = "Interest % (including cash drag) by week - last 6 months",
    subtitle = str_c("today: ", pb_today),
    x = "Week",
    y = "ROI (% p.a.)"
  )

save_plot("interest-percentage-01-by-week-6-months.png", width = 60)
