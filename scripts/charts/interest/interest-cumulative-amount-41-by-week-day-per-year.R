# ------------------------------------------------------------------------------
# Interest cumulative amount by week days (per year)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    year = year(date),
    week_day = wday(date, label = TRUE)
  ) |> 
  group_by(year, week_day) |> 
  summarize(
    interest_amount = sum(amount),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = week_day, y = interest_amount)) +
  geom_col(width = .75) +
  geom_text(
    aes(label = interest_amount),
    vjust = -0.5,
    size = 3
  ) +
  facet_wrap(~ year, ncol = 1) +
  scale_y_continuous(
    expand = expand_scale(mult = c(0.05, 0.1)),
    n.breaks = 10,
    minor_breaks = F
  ) +
  labs(
    title = "Interest cumulative amount by week days (per year)",
    subtitle = str_c("today: ", pb_today),
    x = "Week day",
    y = "Amount (€)"
  )

save_plot("interest/interest-cumulative-amount-41-by-week-day-per-year.png")
save_plot("interest-cumulative-amount-41-by-week-day-per-year.png")
