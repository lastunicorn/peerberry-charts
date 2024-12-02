# ------------------------------------------------------------------------------
# Uninvested amount by month (per year)

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
  add_row(
    month_as_date = generate_monthly_dates(pb_transactions.first_year_as_date, pb_transactions.last_year_as_date + months(11)),
    uninvested_percentage_average = 0
  ) |>
  mutate(
    year = year(month_as_date),
    month = factor(format(month_as_date, "%b"), month.abb, ordered = T)
  ) |> 
  ggplot(aes(x = month, y = uninvested_percentage_average)) +
  geom_col(width = .7) +
  geom_text(
    aes(label = if_else(uninvested_percentage_average == 0, NA, paste(format(round(uninvested_percentage_average, 2), nsmall = 2), "%"))),
    vjust = -0.5,
    size = 3
  ) +
  facet_wrap(~ year, ncol = 1, scales = "free_x") +
  scale_y_continuous(
    n.breaks = 20,
    minor_breaks = F,
    expand = expand_scale(mult = c(0.05, 0.1))
  ) +
  labs(
    title = "Cash drag percentage average by month (per year)",
    subtitle = str_c("today: ", pb_today),
    x = "Month",
    y = "Cash drag (%)"
  )

save_plot("uninvested/uninvested-04-percentage-by-month-per-year.png")
save_plot("uninvested-04-percentage-by-month-per-year.png")
