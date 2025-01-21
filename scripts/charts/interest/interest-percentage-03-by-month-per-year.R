# ------------------------------------------------------------------------------
# ROI - Interest % by month - per year

pb_transactions |> 
  mutate(
    interest = if_else(type %in% c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"), amount, 0)
  ) |> 
  group_by(date) |> 
  summarize(
    interest = sum(interest),
    funds_delta = sum(funds_delta)
  ) |> 
  arrange(date) |> 
  mutate(
    funds = cumsum(funds_delta),
    month_as_date = floor_date(date, "month")
  ) |> 
  add_row(
    month_as_date = generate_monthly_dates(pb_transactions.first_year_as_date, pb_transactions.last_year_as_date + months(11)),
    interest = 0
  ) |>
  arrange(date) |> 
  mutate(
    investment_days = if_else(is.na(lag(date)), 0, as.numeric(date - lag(date))),
    interest_percentage_peranum = interest_percentage_peranum(lag(date), date, lag(funds), interest)
  ) |> 
  group_by(month_as_date) |> 
  mutate(
    days_in_month = pb.days_in_month(date)
  ) |> 
  summarize(
    roi = sum(interest_percentage_peranum * investment_days / first(days_in_month))
  ) |> 
  mutate(
    year = as.factor(year(month_as_date)),
    month = factor(format(month_as_date, "%b"), month.abb, ordered = T)
  ) |> 
  ggplot(aes(x = month, y = roi)) +
  geom_col(width = .5) +
  geom_text(
    aes(label = paste(format(round(roi, 2), nsmall = 2), "%")),
    vjust = -0.5,
    size = 3
  ) +
  facet_wrap(~ year, ncol = 1, scales = "free_x") +
  scale_y_continuous(
    expand = expand_scale(mult = c(0.05, 0.1)),
    n.breaks = 10,
    minor_breaks = F
  ) +
  labs(
    title = "Interest % (including cash drag) by month (per year)",
    subtitle = str_c("today: ", pb_today),
    x = "Month",
    y = "ROI (% p.a.)"
  )

save_plot("interest/interest-percentage-03-by-month-per-year.png")
save_plot("interest-percentage-03-by-month-per-year.png")
