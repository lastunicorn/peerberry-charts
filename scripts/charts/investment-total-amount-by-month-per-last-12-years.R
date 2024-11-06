# ------------------------------------------------------------------------------
# Invested amount of money by month in last 12 months per country.

pb_transactions |>  
  filter(
    type == "INVESTMENT",
    date >= floor_date(today(), "month") - months(11)
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
  labs(
    title = "Investment amount by month in last 12 months (per country)",
    x = "Purchase month",
    y = "Amount (€)"
  )

save_plot("investment-amount-by-month-per-country-12.png")
