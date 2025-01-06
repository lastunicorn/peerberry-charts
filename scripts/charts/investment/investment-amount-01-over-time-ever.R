# ------------------------------------------------------------------------------
# Individual investment amount over time (ever)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(year = year(date)) |> 
  mutate(year = as.factor(year)) |> 
  ggplot(aes(x = date, y = amount, color = year)) +
  geom_point(size = .7, position = "jitter")  +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Individual investment amount over time (ever)",
    subtitle = str_c("today: ", pb_today),
    x = "Date",
    y = "Investment size (€)"
  )

save_plot("investment/investment-amount-01-over-time-ever.png")
