library(tidyverse)


# ------------------------------------------------------------------------------
# Investment count by length (days) in 2024 (per country).

temp <- pb |> 
  mutate(
    estimated_days = as.integer(estimated_final_payment_date - date_of_purchase),
    purchese_year = year(date_of_purchase)
  ) |> 
  filter(purchese_year == 2024)

temp |> 
  ggplot(aes(x = estimated_days)) +
  geom_bar() +
  facet_wrap(~ country, ncol = 1) +
  scale_x_continuous(breaks = round(seq(0, max(temp$estimated_days), by = 2), 1)) +
  labs(
    title = "Investment count by length (days) in 2024 (per country)",
    x = "Days",
    y = "Count"
  )

remove(temp)

save_plot("investment-count-by-length-per-country-2024.png", width=30, height=60)
