# ------------------------------------------------------------------------------
# Invested cumulative amount by month in last 12 months per country.

pb_transactions |>  
  filter(
    type == "INVESTMENT",
    date >= pb_transactions.current_month_as_date - months(11)
  ) |> 
  mutate(
    month_as_date = floor_date(date, "month")
  ) |>
  group_by(country, month_as_date) |> 
  summarize(
    sum = sum(amount),
    .groups = "drop"
  ) |> 
  arrange(month_as_date) |> 
  mutate(
    x = format(month_as_date, "%b %Y")
  ) |> 
  ggplot(aes(x = reorder(x, month_as_date), y = sum)) +
  geom_col() +
  facet_wrap(~ country) +
  guides(x = guide_axis(angle = 70)) +
  theme(axis.text = element_text(size = 7)) +
  scale_y_continuous(
    n.breaks = 10,
    minor_breaks = F
  ) +
  labs(
    title = "Investment amount by month (per country) - last 12 months",
    subtitle = str_c("today: ", pb_today),
    x = "Purchase month",
    y = "Amount (€)"
  )

save_plot("investment/investment-cumulative-amount-04-by-month-per-country-12.png")
save_plot("investment-cumulative-amount-04-by-month-per-country-12.png")
