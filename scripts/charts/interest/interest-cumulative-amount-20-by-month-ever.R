# ------------------------------------------------------------------------------
# Interest cumulative amount by month (ever)

pb_transactions |>
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    month_as_date = floor_date(date, "month")
  ) |>
  group_by(month_as_date) |>
  arrange(date) |>
  summarize(interest_amount = sum(amount)) |>
  ggplot(aes(x = month_as_date, y = interest_amount)) +
  geom_col() +
  geom_text(aes(label = interest_amount), vjust = -0.5, size = 3) +
  guides(x = guide_axis(angle = 60)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  labs(
    title = "Interest cumulative amount by month (ever)",
    subtitle = str_c("today: ", pb_today),
    x = "Month",
    y = "Amount (€)"
  )

save_plot("interest/interest-cumulative-amount-20-by-month-ever.png")
