# ------------------------------------------------------------------------------
# Principal cumulative amount returned by day (ever)

pb_transactions |>
  filter(is.element(type, c("BUYBACK_PRINCIPAL", "REPAYMENT_PRINCIPAL"))) |>
  group_by(date) |>
  arrange(date) |>
  summarize(
    interest_amount = sum(amount)
  ) |>
  add_row(
    date = ymd("2023-01-01"),
    interest_amount = 0
  ) |>
  add_row(
    date = ymd("2024-12-31"),
    interest_amount = 0
  ) |> 
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
  labs(
    title = "Principal cumulative amount returned by day (per year)",
    x = "Date",
    y = "Amount (€)"
  )

save_plot("principal/principal-cumulative-amount-by-day-per-year.png")
