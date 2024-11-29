# ------------------------------------------------------------------------------
# Principal cumulative amount returned by day (ever)

pb_transactions |>
  filter(is.element(type, c("BUYBACK_PRINCIPAL", "REPAYMENT_PRINCIPAL"))) |>
  group_by(date) |>
  arrange(date) |>
  summarize(interest_amount = sum(amount)) |>
  ggplot(aes(x = date)) +
  geom_col(aes(y = interest_amount)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Principal cumulative amount returned by day (ever)",
    x = "Date",
    y = "Amount (€)"
  )

save_plot("principal/principal-cumulative-amount-by-day-ever.png")
