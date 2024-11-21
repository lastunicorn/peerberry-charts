# ------------------------------------------------------------------------------
# Uninvested amount by month (ever)

pb_transactions |> 
  arrange(date) |> 
  mutate(
    amount_with_sign = if_else(type == "INVESTMENT", -amount, amount),
    uninvested_amount = cumsum(amount_with_sign)
  ) |> 
  group_by(date) |> 
  summarize(
    uninvested_amount = last(uninvested_amount)
  ) |> 
  mutate(
    month_as_date = floor_date(date, "month"),
    days_in_month = lubridate::days_in_month(month_as_date)
  ) |> 
  group_by(month_as_date) |>
  summarize(
    uninvested_amount = sum(uninvested_amount) / first(days_in_month)
  ) |> 
  ggplot(aes(x = month_as_date, y = uninvested_amount)) +
  geom_col() +
  geom_text(aes(label = format(round(uninvested_amount, 2), nsmall = 2)), vjust = -0.5, size = 3) +
  guides(x = guide_axis(angle = 60)) +
  scale_y_continuous(
    n.breaks = 20,
    minor_breaks = F
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    minor_breaks = NULL
  ) +
  labs(
    title = "Cash drag average by month (ever)",
    x = "Month",
    y = "Cash drag (€)"
  )

save_plot("uninvested-03-amount-by-month-ever.png")
