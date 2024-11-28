# ------------------------------------------------------------------------------
# Invested cumulative amount by month (per year and country)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(
    month = month(date),
    year = year(date)
  ) |>
  mutate(month = as.factor(month)) |> 
  group_by(country, year, month) |> 
  summarize(
    sum = sum(amount),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = month, y = sum)) +
  geom_col() +
  facet_grid(country ~ year) +
  labs(
    title = "Investment amount by month (per year and country)",
    subtitle = str_c("today: ", pb_today),
    x = "Purchase month",
    y = "Amount (€)"
  )

save_plot("investment-cumulative-amount-by-month-per-country-and-year.png", width = 20, height = 30)
