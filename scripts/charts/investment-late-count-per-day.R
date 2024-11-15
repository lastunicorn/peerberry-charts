# ------------------------------------------------------------------------------
# Late investments count per day

pb_loans |> 
  filter(status != "FINISHED") |> 
  mutate(
    remaining_days = estimated_final_payment_date - today(),
    .keep = "used"
  ) |> 
  mutate(
    late_days = -remaining_days
  ) |> 
  group_by(late_days) |> 
  summarize(
    n = n(),
    is_late = first(late_days) >= 0
  ) |> 
  ggplot(aes(x = late_days, y = n, fill = is_late)) +
  geom_col() +
  geom_text(
    aes(label = n),
    vjust = -0.5,
    size = 3
  ) +
  scale_x_continuous(
    n.breaks = 60,
    minor_breaks = F
  ) +
  labs(
    title = str_c("Late investments per day"),
    subtitle = str_c("today: ", today()),
    x = "Days",
    y = "Count"
  )

save_plot("investment-late-count-per-day.png", width = 60)
