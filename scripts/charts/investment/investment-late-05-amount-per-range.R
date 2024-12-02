# ------------------------------------------------------------------------------
# Late investments amount per range

pb_loans |> 
  filter(status != "FINISHED") |> 
  mutate(
    remaining_days = as.numeric(estimated_final_payment_date - pb_today),
    late_category = pb.late_category(remaining_days)
  ) |> 
  group_by(late_category) |> 
  summarize(
    invested_amount = sum(invested_amount)
  ) |> 
  mutate(
    invested_percentage = invested_amount * 100 / sum(invested_amount)
  ) |> 
  ggplot(aes(x = late_category, y = invested_amount)) +
  geom_col(width = .7) +
  geom_text(
    aes(label = str_c(round(invested_amount, 2), " €\n(", round(invested_percentage, 2), " %)")),
    vjust = -0.4,
    size = 3
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    expand = expand_scale(mult = c(0.05, 0.1))
  ) +
  labs(
    title = "Late investments per ranges",
    subtitle = str_c("today: ", pb_today),
    x = "Days",
    y = "Amount (€)"
  )

save_plot("investment/investment-late-05-amount-per-range.png")
save_plot("investment-late-05-amount-per-range.png")
