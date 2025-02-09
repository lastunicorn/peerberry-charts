# ------------------------------------------------------------------------------
# Interest cumulative amount by week days (per month) - 12 months

pb_transactions |> 
  filter(date >= pb_transactions.last_month_as_date - months(11)) |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    month_as_date = floor_date(date, "month"),
    week_day = wday(date, label = TRUE)
  ) |> 
  group_by(month_as_date, week_day) |> 
  summarize(
    interest_amount = sum(amount),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = week_day, y = interest_amount)) +
  geom_col(width = .75) +
  geom_text(
    aes(label = interest_amount),
    vjust = -0.5,
    size = 3,
    color = "#666"
  ) +
  facet_wrap(
    ~ month_as_date,
    labeller = as_labeller(month_year_labeller)
  ) +
  scale_y_continuous(
    expand = expand_scale(mult = c(0.05, 0.1)),
    n.breaks = 10,
    minor_breaks = F
  ) +
  labs(
    title = "Interest cumulative amount by week days (per month) - 12 months",
    subtitle = str_c("today: ", pb_today),
    x = "Week day",
    y = "Amount (€)"
  )

save_plot("interest/interest-cumulative-amount-40-by-week-day-per-month.png")
save_plot("interest-cumulative-amount-40-by-week-day-per-month.png")
