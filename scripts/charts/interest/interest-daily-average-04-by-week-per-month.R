# ------------------------------------------------------------------------------
# Interest daily average amount by week days (per month)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  group_by(date) |> 
  summarize(
    amount = sum(amount)
  ) |> 
  mutate(
    month_as_date = floor_date(date, "month"),
    week_day = wday(date, label = TRUE)
  ) |> 
  group_by(month_as_date, week_day) |> 
  summarize(
    interest_average = mean(amount),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = week_day, y = interest_average)) +
  geom_col(width = .75) +
  geom_text(
    aes(label = format(round(interest_average, 2), nsmall = 2)),
    vjust = -0.5,
    size = 3,
    color = "#666"
  ) +
  facet_wrap(~ month_as_date, labeller = as_labeller(month_year_labeller)) +
  labs(
    title = "Interest daily average amount by week days (per month)",
    subtitle = str_c("today: ", pb_today),
    x = "Week day",
    y = "Amount (€)"
  )

save_plot("interest/interest-daily-average-04-by-week-day-per-month.png")
