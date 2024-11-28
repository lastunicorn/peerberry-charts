# ------------------------------------------------------------------------------
# Total interest over time (ever)

pb_transactions |> 
  arrange(date) |> 
  mutate(
    amount3 = ifelse(type %in% c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"), amount, 0),
    total_interest = map_dbl(date, ~ sum(amount3[date <= .x]))
  ) |>
  uncount(2) |> 
  mutate(
    total_interest = lag(total_interest)
  ) |> 
  slice_tail(n = -1) |> 
  ggplot(aes(x = date)) +
  geom_area(aes(y = total_interest), alpha = .7) +
  geom_line(aes(y = total_interest), alpha = .5) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    n.breaks = 20,
    minor_breaks = F
  ) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Total interest over time (ever)",
    subtitle = str_c("today: ", pb_today),
    x = "Date",
    y = "Amount (€)"
  )

save_plot("interest-total-over-time-ever.png")
