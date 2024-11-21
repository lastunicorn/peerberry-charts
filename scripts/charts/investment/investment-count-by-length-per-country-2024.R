library(tidyverse)


# ------------------------------------------------------------------------------
# Investment count by length (days) in 2024 (per country).

pb_loans |> 
  mutate(
    estimated_days = as.integer(estimated_final_payment_date - date_of_purchase),
    purchese_year = year(date_of_purchase)
  ) |> 
  filter(purchese_year == 2024) |> 
  ggplot(aes(x = estimated_days)) +
  geom_bar() +
  facet_wrap(~ country, ncol = 2) +
  scale_x_continuous(
    n.breaks = 30
  ) +
  expand_limits(x = 0) +
  labs(
    title = "Investment count by length (days) in 2024 (per country)",
    x = "Days",
    y = "Count"
  )

save_plot("investment-count-by-length-per-country-2024.png")
