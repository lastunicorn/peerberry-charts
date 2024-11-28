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
  scale_x_date(date_breaks = "1 month", date_labels = "%b", minor_breaks = NULL) +
  labs(
    title = "Individual investment amount over time (per year)",
    x = "Date",
    y = "Investment size (€)"
  )

save_plot("investment-amount-over-time-per-year.png")
