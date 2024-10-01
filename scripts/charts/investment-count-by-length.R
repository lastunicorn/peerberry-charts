library(tidyverse)


# ------------------------------------------------------------------------------
# Investments count by length (days) (per year)

temp <- pb |> 
  mutate(
    estimated_time = as.integer(estimated_final_payment_date - date_of_purchase),
    purchese_year = year(date_of_purchase)
  ) |> 
  arrange(purchese_year)

temp |> 
  ggplot(aes(x = estimated_time)) +
  geom_bar() +
  facet_wrap(~ purchese_year, ncol = 1) +
  scale_x_continuous(breaks = round(seq(0, max(temp$estimated_time), by = 2), 1)) +
  labs(
    title = "Investment count by length (days) (per year)",
    x = "Days",
    y = "Count"
  )

remove(temp)

# Save
ggsave("charts/investment-count-by-length-per-year.png", width=30, height=20, units="cm", dpi=300)



# ------------------------------------------------------------------------------
# Investment count by length (days) in 2024 (per country).

temp <- pb |> 
  mutate(
    estimated_time = as.integer(estimated_final_payment_date - date_of_purchase),
    purchese_year = year(date_of_purchase)
  ) |> 
  filter(purchese_year == 2024) |> 
  arrange(purchese_year)

temp |> 
  ggplot(aes(x = estimated_time)) +
  geom_bar() +
  facet_wrap(~ country, ncol = 1) +
  scale_x_continuous(breaks = round(seq(0, max(temp$estimated_time), by = 2), 1)) +
  labs(
    title = "Investment count by length (days) in 2024 (per country)",
    x = "Days",
    y = "Count"
  )

remove(temp)

# Save
ggsave("charts/investment-count-by-length-per-country-2024.png", width=30, height=60, units="cm", dpi=300)