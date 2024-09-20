library(tidyverse)


# ------------------------------------------------------------------------------
# Received interest amount by number of investment days.

pb |>
  filter(status == "FINISHED") |> 
  mutate(
    interest = received_payments - invested_amount,
    days = last_received_payment_date - date_of_purchase,
    purchase_year = year(date_of_purchase)
  ) |>
  ggplot(aes(x = days, y = interest)) +
  geom_point() +
  facet_wrap(~purchase_year) +
  labs(
    title = "Interest amount by investment days count",
    x = "Investment days",
    y = "Interest"
  )

# Save
ggsave("charts/interest-amount-by-investment-days-per-year.png", width=30, height=20, units="cm", dpi=300)


# ------------------------------------------------------------------------------
# One chart per country:
#   - Received interest amount by number of investment days.

pb |>
  filter(status == "FINISHED") |> 
  mutate(
    interest = received_payments - invested_amount,
    days = last_received_payment_date - date_of_purchase,
    purchase_year = year(date_of_purchase)) |>
  filter(purchase_year == 2024) |> 
  ggplot(aes(x = days, y = interest)) +
  geom_point() +
  facet_wrap(~country, ncol = 4) +
  labs(
    title = "Interest amount by investment days count (2024)",
    x = "Investment days",
    y = "Interest"
  )

# Save
ggsave("charts/interest-amount-by-investment-days-per-country-in-2024.png", width=30, height=20, units="cm", dpi=300)
