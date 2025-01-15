# ------------------------------------------------------------------------------
# Individual investment amount over time (per year)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(
    year = year(date),
    date_without_year = make_date(2000, month(date), day(date))
  ) |> 
  ggplot(aes(x = date_without_year, y = amount)) +
  geom_point(color = "gray40", size = .7, position = "jitter") +
  facet_wrap(~ year, ncol = 1) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    n.breaks = 10,
    minor_breaks = F
  ) +
  labs(
    title = "Individual investment amount over time (per year)",
    subtitle = str_c("today: ", pb_today),
    x = "Date",
    y = "Investment size (€)"
  )

save_plot("investment/investment-amount-02-over-time-per-year.png")
save_plot("investment-amount-02-over-time-per-year.png")
