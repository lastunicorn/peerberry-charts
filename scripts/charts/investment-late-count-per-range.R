library(tidyverse)


# ------------------------------------------------------------------------------
# Late investments count per range

pb_loans |> 
  filter(status != "FINISHED") |> 
  mutate(
    remaining_days = as.numeric(estimated_final_payment_date - today()),
    late_category = case_when(
      remaining_days > 0 ~ "in time",
      remaining_days == 0 ~ "late 0",
      remaining_days >= -15 ~ "late 15",
      remaining_days >= -30 ~ "late 30",
      remaining_days >= -60 ~ "late 60",
      .default = "late-late"
    ),
    .keep = "used"
  ) |> 
  group_by(late_category) |> 
  summarize(n = n()) |> 
  ggplot(aes(x = late_category, y = n)) +
  geom_col(width = .7) +
  geom_text(
    aes(label = n),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = str_c("Late investments per ranges (", today(), ")"),
    x = "Days",
    y = "Count"
  )

save_plot("investment-late-count-per-range.png")
