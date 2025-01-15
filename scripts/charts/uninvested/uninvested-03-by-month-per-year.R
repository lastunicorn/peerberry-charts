# ------------------------------------------------------------------------------
# Uninvested amount by month (per year)

pb_transactions |> 
  arrange(date) |> 
  mutate(
    amount_with_sign = if_else(type == "INVESTMENT", -amount, amount),
    uninvested_amount = cumsum(amount_with_sign)
  ) |> 
  add_row(
    date = generate_monthly_dates(pb_transactions.first_year_as_date, pb_transactions.last_year_as_date + months(11)),
    uninvested_amount = 0
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
  mutate(
    year = year(month_as_date),
    month = factor(format(month_as_date, "%b"), month.abb, ordered = T)
  ) |> 
  ggplot(aes(x = month, y = uninvested_amount)) +
  geom_col(width = .5) +
  geom_text(
    aes(label = if_else(uninvested_amount == 0, NA, format(round(uninvested_amount, 2), nsmall = 2))),
    vjust = -0.5,
    size = 3
  ) +
  facet_wrap(~ year, ncol = 1) +
  scale_y_continuous(
    n.breaks = 10,
    minor_breaks = F,
    expand = expand_scale(mult = c(0.05, 0.1))
  ) +
  labs(
    title = "Cash drag average by month (per year)",
    subtitle = str_c("today: ", pb_today),
    x = "Month",
    y = "Cash drag (€)"
  )

save_plot("uninvested/uninvested-03-amount-by-month-per-year.png")
save_plot("uninvested-03-amount-by-month-per-year.png")
