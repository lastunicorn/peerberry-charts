# ------------------------------------------------------------------------------
# Uninvested amount over time (ever)

pb_transactions |> 
  arrange(date) |> 
  mutate(
    amount2 = ifelse(type == "INVESTMENT", -amount, amount),
    uninvested_amount = map_dbl(date, ~ sum(amount2[date <= .x]))
  ) |>
  group_by(date) |> 
  summarize(
    uninvested_amount = first(uninvested_amount)
  ) |>
  uncount(2) |> 
  mutate(
    uninvested_amount = lag(uninvested_amount)
  ) |> 
  slice_tail(n = -1) |> 
  ggplot(aes(x = date, y = uninvested_amount)) +
  geom_area(fill = "gray60") +
  geom_path() +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    minor_breaks = NULL
  ) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Cash drag over time (ever)",
    subtitle = str_c("today: ", pb_today),
    x = "Date",
    y = "Cash drag (€)"
  )

save_plot("uninvested/uninvested-01-amount-over-time-ever.png")
