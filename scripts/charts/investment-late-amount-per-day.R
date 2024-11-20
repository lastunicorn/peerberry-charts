# ------------------------------------------------------------------------------
# Late investments amount per day

pb_loans |> 
  filter(status != "FINISHED") |> 
  mutate(
    remaining_days = estimated_final_payment_date - pb_today
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
    #n.breaks = 60,
    minor_breaks = F,
    limits = function(x){
      c(-100, 60)
    },
    breaks = seq(from = -100, to = 60, by = 2)
  ) +
  labs(
    title = str_c("Late investments per day"),
    subtitle = str_c("today: ", pb_today),
    x = "Days",
    y = "Amount (€)"
  )

save_plot(str_c("investment-late-amount-per-day-", pb_today, ".png"), width = 60)
