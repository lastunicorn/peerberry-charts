# ------------------------------------------------------------------------------
# Investment cumulative Amount by country (per year)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(
    year = year(date),
    country = fct(country, pb_country_levels)
  ) |> 
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
    subtitle = str_c("today: ", pb_today),
    x = "Country",
    y = "Amount (€)"
  )

save_plot("investment/investment-cumulative-amount-01-by-country-per-year.png")
save_plot("investment-cumulative-amount-01-by-country-per-year.png")
