# ------------------------------------------------------------------------------
# Interest cumulative amount by day (ever)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  group_by(date) |>
  arrange(date) |> 
  summarize(
    interest_amount = sum(amount)
  ) |>
  ggplot(aes(x = date)) +
  geom_col(aes(y = interest_amount)) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    minor_breaks = NULL
  ) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Interest cumulative amount by day (ever)",
    subtitle = str_c("today: ", pb_today),
    x = "Date",
    y = "Amount (€)"
  )

save_plot("interest/interest-cumulative-amount-01-by-day-ever.png")
