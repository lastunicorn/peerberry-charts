# ------------------------------------------------------------------------------
# Late investments amount per day

pb_loans |> 
  filter(status != "FINISHED") |> 
  mutate(
    remaining_days = estimated_final_payment_date - today()
  ) |> 
  mutate(
    late_days = -remaining_days
  ) |> 
  group_by(late_days) |> 
  summarize(
    n = sum(invested_amount),
    is_late = first(late_days) >= 0
  ) |> 
  ggplot(aes(x = late_days, y = n, fill = is_late)) +
  geom_col(width = .7) +
  geom_text(
    aes(label = n),
    hjust = -0.1,
    vjust = -0.1,
    size = 3,
    angle = 45
  ) +
  scale_x_continuous(
    n.breaks = 60,
    minor_breaks = F
  ) +
  labs(
    title = str_c("Late investments per day"),
    subtitle = str_c("today: ", today()),
    x = "Days",
    y = "Amount (€)"
  )

save_plot("investment-late-amount-per-day.png", width = 60)
