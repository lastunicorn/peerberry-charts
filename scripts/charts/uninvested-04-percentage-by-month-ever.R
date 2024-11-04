# ------------------------------------------------------------------------------
# Uninvested amount by month (ever)

pb_transactions |> 
  arrange(date) |> 
  mutate(
    amount_with_sign = if_else(type == "INVESTMENT", -amount, amount),
    uninvested_amount = cumsum(amount_with_sign),
    total_amount = cumsum(if_else(type %in% c("DEPOSIT", "BUYBACK_INTEREST", "REPAYMENT_INTEREST"), amount, 0))
  ) |> 
  group_by(date) |> 
  summarize(
    uninvested_amount = last(uninvested_amount),
    total_amount = last(total_amount)
  ) |> 
  mutate(
    month_as_date = floor_date(date, "month"),
    days_in_month = lubridate::days_in_month(month_as_date)
  ) |> 
  group_by(month_as_date) |>
  summarize(
    uninvested_amount_average = sum(uninvested_amount) / first(days_in_month),
    total_amount_average = sum(total_amount) / first(days_in_month),
    uninvested_percentage_average = uninvested_amount_average * 100 / total_amount_average
  ) |> 
  ggplot(aes(x = month_as_date, y = uninvested_percentage_average)) +
  geom_col() +
  geom_text(aes(label = paste(format(round(uninvested_percentage_average, 2), nsmall = 2), "%")), vjust = -0.5, size = 3) +
  guides(x = guide_axis(angle = 60)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  scale_y_continuous(n.breaks = 20, minor_breaks = F) +
  labs(
    title = "Cash drag percentage average by month (ever)",
    x = "Month",
    y = "Cash drag (%)"
  )

save_plot("uninvested-04-percentage-by-month-ever.png")
