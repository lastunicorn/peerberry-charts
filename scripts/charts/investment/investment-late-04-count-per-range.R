# ------------------------------------------------------------------------------
# Late investments count per range

pb_loans |> 
  filter(status != "FINISHED") |> 
  mutate(
    remaining_days = as.numeric(estimated_final_payment_date - pb_today),
    late_category = pb.late_category(remaining_days),
    .keep = "used"
  ) |> 
  group_by(late_category) |> 
  summarize(
    n = n()
  ) |> 
  ggplot(aes(x = late_category, y = n)) +
  geom_col(width = .7) +
  geom_text(
    aes(label = n),
    vjust = -0.5,
    size = 3
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    n.breaks = 20,
    minor_breaks = F
  ) +
  labs(
    title = "Late investments per ranges",
    subtitle = str_c("today: ", pb_today),
    x = "Days",
    y = "Count"
  )

save_plot("investment/investment-late-04-count-per-range.png")
