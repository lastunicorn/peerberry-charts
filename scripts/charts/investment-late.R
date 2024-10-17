library(tidyverse)


# ------------------------------------------------------------------------------
# Late investments

pb_loans |> 
  filter(status != "FINISHED") |> 
  mutate(
    remaining_days = estimated_final_payment_date - today(),
    .keep = "used"
  ) |> 
  filter(remaining_days <= 0) |> 
  mutate(remaining_days = -remaining_days) |> 
  group_by(remaining_days) |> 
  summarize(n = n()) |> 
  ggplot(aes(x = remaining_days, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  labs(
    title = "Late investments",
    x = "Days",
    y = "Count"
  )
  