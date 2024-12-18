# ------------------------------------------------------------------------------
# Interest cumulative amount daily (per year)

pb_transactions |>
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  pb_transactions.add_start_end_dates() |> 
  group_by(date) |>
  arrange(date) |> 
  summarize(interest_amount = sum(amount)) |>
  mutate(
    year = year(date)
  ) |>
  ggplot(aes(x = date)) +
  geom_col(aes(y = interest_amount)) +
  facet_wrap(~ year, ncol = 1, scales = "free_x") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    minor_breaks = NULL
  ) +
  scale_y_continuous(n.breaks = 15, minor_breaks = F) +
  labs(
    title = "Interest cumulative amount by day (per year)",
    subtitle = str_c("today: ", pb_today),
    x = "Date",
    y = "Amount (€)"
  )

save_plot("interest/interest-cumulative-amount-03-by-day-per-year.png")
save_plot("interest-cumulative-amount-03-by-day-per-year.png")
