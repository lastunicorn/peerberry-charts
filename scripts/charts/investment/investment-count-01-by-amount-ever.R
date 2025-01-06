# ------------------------------------------------------------------------------
# Invested count by amount (ever)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  ggplot(aes(x = amount)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(
    breaks = round(seq(min(pb_transactions$amount), max(pb_transactions$amount), by = 20), 1)
  ) +
  labs(
    title = "Investment count by amount (ever)",
    subtitle = str_c("today: ", pb_today),
    x = "Investment size (€)",
    y = "Investments count"
  )

save_plot("investment/investment-count-01-by-amount-ever.png")
