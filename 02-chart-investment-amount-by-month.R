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

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(
    month = month(date),
    year = year(date)
  ) |>
  mutate(month = as.factor(month)) |> 
  group_by(country, year, month) |> 
  summarize(
    sum = sum(amount),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = month, y = sum)) +
  geom_col() +
  facet_grid(country ~ year) +
  labs(
    title = "Investments amount by month (per year and country)",
    x = "Purchease month",
    y = "Amount"
  )

# Save
ggsave("charts/investment-amount-by-month-in-each-country.png", width=20, height=60, units="cm", dpi=300)
