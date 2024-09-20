library(tidyverse)


# ------------------------------------------------------------------------------
# One chart per country:
#   - Display invested amount of money by month in 2024.

pb |> 
  mutate(
    purchase_month = month(date_of_purchase),
    purchase_year = year(date_of_purchase)
  ) |> 
  mutate(purchase_month = as.factor(purchase_month)) |> 
  filter(purchase_year == 2024) |> 
  group_by(country, purchase_month) |> 
  arrange(country) |> 
  summarize(
    sum = sum(invested_amount)
  ) |> 
  ggplot(aes(x = purchase_month, y = sum)) +
  geom_col() +
  facet_wrap(~country) +
  labs(
    title = "Investments amount by month in 2024 (per country)",
    x = "Purchease month",
    y = "Amount"
  )

# Save
ggsave("charts/investment-amount-by-month-in-each-country-2024.png", width=30, height=20, units="cm", dpi=300)



# ------------------------------------------------------------------------------
# One chart per country and year:
#   - Display invested amount of money by month.

pb |> 
  mutate(
    purchase_month = month(date_of_purchase),
    purchase_year = year(date_of_purchase)
  ) |> 
  mutate(purchase_month = as.factor(purchase_month)) |> 
  group_by(country, purchase_year, purchase_month) |> 
  arrange(country) |> 
  summarize(
    sum = sum(invested_amount)
  ) |> 
  ggplot(aes(x = purchase_month, y = sum)) +
  geom_col() +
  facet_grid(country ~ purchase_year) +
  labs(
    title = "Investments amount by month (in each year and country)",
    x = "Purchease month",
    y = "Amount"
  )

# Save
ggsave("charts/investment-amount-by-month-in-each-country.png", width=20, height=60, units="cm", dpi=300)
