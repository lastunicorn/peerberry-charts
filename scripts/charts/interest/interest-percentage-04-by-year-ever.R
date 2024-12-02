# ------------------------------------------------------------------------------
# ROI - Interest % by month - ever

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
    year = as.factor(year(date)),
    start_date = min(date),
    end_date = max(date) + days(1),
    roi_percentage = if_else(is.na(lag(funds)), 0, interest * 100 / lag(funds)),
    investment_days = if_else(is.na(lag(date)), 0, as.numeric(date - lag(date))),
    roi_percentage_pa = if_else(investment_days == 0, 0, roi_percentage * 365 / investment_days)
  ) |> 
  group_by(year) |> 
  mutate(
    year_start_date = floor_date(date, "year"),
    year_start_date = if_else(year_start_date < start_date, start_date, year_start_date),
    year_end_date = ceiling_date(date, "year"),
    year_end_date = if_else(year_end_date > end_date, end_date, year_end_date),
    days_in_year = (year_start_date %--% year_end_date) / days(1)
  ) |> 
  summarize(
    roi = sum(roi_percentage_pa * investment_days / first(days_in_year))
  ) |> 
  ggplot(aes(x = year, y = roi)) +
  geom_col(width = .5) +
  geom_text(
    aes(label = paste(format(round(roi, 2), nsmall = 2), "%")),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Interest % (including cash drag) by year",
    subtitle = str_c("today: ", pb_today),
    x = "Month",
    y = "ROI (% p.a.)"
  )

save_plot("interest/interest-percentage-04-by-year-ever.png")
save_plot("interest-percentage-04-by-year-ever.png")
