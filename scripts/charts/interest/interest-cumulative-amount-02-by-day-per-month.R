# ------------------------------------------------------------------------------
# Interest cumulative amount daily (per month) - last 12 months

pb_transactions |> 
  filter(date >= pb_transactions.last_month_as_date - months(11)) |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  group_by(date) |>
  arrange(date) |> 
  summarize(interest_amount = sum(amount)) |>
  mutate(
    year = year(date),
    month = floor_date(date, "month"),
    day = day(date)
  ) |>
  ggplot(aes(x = day)) +
  geom_col(aes(y = interest_amount)) +
  facet_wrap(~ month, labeller = as_labeller(month_year_labeller)) +
  scale_x_continuous(
    n.breaks = 10,
    minor_breaks = F
  ) +
  scale_y_continuous(
    n.breaks = 10,
    minor_breaks = F
  ) +
  labs(
    title = "Interest cumulative amount by day (per month) - 12 months",
    subtitle = str_c("today: ", pb_today),
    x = "Date",
    y = "Amount (€)"
  )

save_plot("interest/interest-cumulative-amount-02-by-day-per-month.png")
save_plot("interest-cumulative-amount-02-by-day-per-month.png")
