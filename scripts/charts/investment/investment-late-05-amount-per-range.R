# ------------------------------------------------------------------------------
# Late investments amount per range

pb_loans |> 
  filter(status != "FINISHED") |> 
  mutate(
    remaining_days = as.numeric(estimated_final_payment_date - pb_today),
    late_category = fct(
      case_when(
        remaining_days >= 0 ~ "in time",
        remaining_days >= -15 ~ "late 1-15",
        remaining_days >= -30 ~ "late 16-30",
        remaining_days >= -60 ~ "late 31-60",
        .default = "late-late"
      ),
      levels = c("in time", "late 1-15", "late 16-30", "late 31-60")
    )
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

save_plot("investment-late-05-amount-per-range.png")
