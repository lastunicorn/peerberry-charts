# ------------------------------------------------------------------------------
# Late investments count per day

pb_loans |> 
  filter(status != "FINISHED") |> 
  mutate(
    remaining_days = estimated_final_payment_date - pb_today,
    .keep = "used"
  ) |> 
  mutate(
    late_days = -remaining_days
  ) |> 
  group_by(late_days) |> 
  summarize(
    n = n(),
    is_late = first(late_days) > 0
  ) |> 
  ggplot(aes(x = late_days, y = n, fill = is_late)) +
  geom_col(width = .7) +
  geom_text(
    aes(label = n),
    vjust = -0.5,
    size = 3
  ) +
  scale_x_continuous(
    minor_breaks = F,
    limits = function(x){
      c(-100, 60)
    },
    breaks = seq(from = -100, to = 60, by = 2)
  ) +
  scale_fill_manual(
    values = c("#30e5a2", "#f39383"),
    labels = c("Current", "Late")
  ) +
  scale_y_continuous(
    expand = expand_scale(mult = c(0.05, 0.075)),
    n.breaks = 20,
    minor_breaks = F
  ) +
  labs(
    title = str_c("Late investments per day"),
    subtitle = str_c("today: ", pb_today),
    x = "Days",
    y = "Count",
    fill = "Status"
  )

save_plot("investment/investment-late-03-count-per-day.png", width = 60)
save_plot("investment-late-03-count-per-day.png", width = 60)
save_plot(str_c("history-investment-late-count/investment-late-count-per-day-", pb_today, ".png"), width = 60)
