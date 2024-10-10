library(tidyverse)


# ------------------------------------------------------------------------------
# One chart per country:
#   - Display invested amount of money by month in 2024.

pb_transactions |>  
  filter(type == "INVESTMENT") |> 
  mutate(
    month = month(date),
    year = year(date)
  ) |>
  mutate(month = as.factor(month)) |> 
  filter(year == 2024) |> 
  group_by(country, month) |> 
  summarize(
    sum = sum(amount),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = month, y = sum)) +
  geom_col() +
  facet_wrap(~ country) +
  scale_x_discrete(limits = sapply(1:12, toString)) +
  labs(
    title = "Investment amount by month in 2024 (per country)",
    x = "Purchease month",
    y = "Amount (€)"
  )

save_plot("investment-amount-by-month-per-country-2024.png")
