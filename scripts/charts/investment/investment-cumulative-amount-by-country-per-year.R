# ------------------------------------------------------------------------------
# Investment cumulative Amount by country (per year)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(year = year(date)) |> 
  group_by(year, country) |> 
  summarize(
    sum = sum(amount),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = country, y = sum)) +
  geom_col() +
  geom_text(
    aes(label = sum),
    vjust = -0.5,
    size = 3
  ) +
  facet_wrap(~ year, ncol = 1) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Investment amount by country (per year)",
    x = "Country",
    y = "Amount (€)"
  )

save_plot("investment-cumulative-amount-by-country-per-year.png")
