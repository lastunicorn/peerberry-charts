# ------------------------------------------------------------------------------
# Late investments count per range

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
    ),
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
  labs(
    title = "Late investments per ranges",
    subtitle = str_c("today: ", pb_today),
    x = "Days",
    y = "Count"
  )

save_plot("investment-late-04-count-per-range.png")
