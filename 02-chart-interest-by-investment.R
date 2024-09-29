library(tidyverse)


# ------------------------------------------------------------------------------
# Received interest amount by number of investment days (ever).

pb |>
  filter(status == "FINISHED") |> 
  mutate(
    interest = received_payments - invested_amount,
    days = last_received_payment_date - date_of_purchase,
    purchase_year = year(date_of_purchase)) |>
  ggplot(aes(x = days, y = interest)) +
  geom_point(color = "#666") +
  labs(
    title = "Interest amount by investment days count (ever)",
    x = "Investment days",
    y = "Interest"
  )

# Save
ggsave("charts/interest-amount-by-investment-days-ever.png", width=30, height=20, units="cm", dpi=300)


# ------------------------------------------------------------------------------
# Received interest amount by number of investment days (per year)

pb |>
  filter(status == "FINISHED") |> 
  mutate(
    interest = received_payments - invested_amount,
    days = last_received_payment_date - date_of_purchase,
    purchase_year = year(date_of_purchase)
  ) |>
  ggplot(aes(x = days, y = interest)) +
  geom_point(color = "#666") +
  facet_wrap(~ purchase_year) +
  labs(
    title = "Interest amount by investment days count (per year)",
    x = "Investment days",
    y = "Interest"
  )

# Save
ggsave("charts/interest-amount-by-investment-days-per-year.png", width=30, height=20, units="cm", dpi=300)


# pb_transactions |> 
#   filter(!(type == "DEPOSIT")) |>
#   group_by(loan_id) |>
#   filter(type == "BUYBACK_INTEREST" | type == "REPAYMENT_INTEREST") |> 
#   summarize(
#     n = n(),
#     sum = sum(amount)
#   ) |> 
#   filter(n > 1)
# 
# pb_transactions |> 
#   filter(loan_id == "12746335")
# 
# 
# pb_transactions |> 
#   filter(!(type == "DEPOSIT")) |>
#   group_by(country) |>
#   summarize(n = n())


# ------------------------------------------------------------------------------
# One chart per country:
#   - Received interest amount by number of investment days (in 2024).

pb |>
  filter(status == "FINISHED") |> 
  mutate(
    interest = received_payments - invested_amount,
    days = last_received_payment_date - date_of_purchase,
    purchase_year = year(date_of_purchase)) |>
  filter(purchase_year == 2024) |> 
  ggplot(aes(x = days, y = interest)) +
  geom_point(color = "#666") +
  facet_wrap(~ country, ncol = 4) +
  labs(
    title = "Interest amount by investment days count (2024)",
    x = "Investment days",
    y = "Interest"
  )

# Save
ggsave("charts/interest-amount-by-investment-days-per-country-in-2024.png", width=30, height=20, units="cm", dpi=300)


# ------------------------------------------------------------------------------
# Received interest amount by number of investment days (Czech Republic - ever).

pb |>
  filter(status == "FINISHED") |> 
  mutate(
    interest = received_payments - invested_amount,
    days = last_received_payment_date - date_of_purchase,
    purchase_year = year(date_of_purchase)) |>
  filter(country == "Czech Republic") |> 
  ggplot(aes(x = days, y = interest)) +
  geom_point(color = "#666") +
  labs(
    title = "Interest amount by investment days count (Czech Republic - ever)",
    x = "Investment days",
    y = "Interest"
  )

# Save
ggsave("charts/interest-amount-by-investment-days-czech-ever.png", width=30, height=20, units="cm", dpi=300)






