# ------------------------------------------------------------------------------
# Invested count by amount (per year)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(year = year(date)) |>
  ggplot(aes(x = amount)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ year, ncol = 1) +
  scale_x_continuous(
    breaks = round(seq(min(pb_transactions$amount), max(pb_transactions$amount), by = 20), 1)
  ) +
  labs(
    title = "Investment count by amount (per year)",
    subtitle = str_c("today: ", pb_today),
    x = "Investment size (€)",
    y = "Investments count"
  )

save_plot("investment/investment-count-02-by-amount-per-year.png")
