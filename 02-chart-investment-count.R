library(tidyverse)


# ------------------------------------------------------------------------------
# One chart per year:
#   - Display investments count in each country.

# Starting from investments
pb |> 
  mutate(purchase_year = year(date_of_purchase)) |> 
  arrange(purchase_year) |> 
  ggplot(aes(x = country)) +
  geom_bar() +
  facet_wrap(~purchase_year) +
  guides(x = guide_axis(angle = 90)) +
  labs(
    title = "Investment count by year and country",
    x = "Country",
    y = "Count"
  )

# Starting from transactions
pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(year = year(date)) |> 
  arrange(year) |> 
  group_by(country) |> 
  ggplot(aes(x = country)) +
  geom_bar() +
  facet_wrap(~year) +
  guides(x = guide_axis(angle = 90)) +
  labs(
    title = "Investment count by year and country",
    x = "Country",
    y = "Count"
  )

# Save
ggsave("charts/investment-count-by-country-in-each-year.png", width=30, height=20, units="cm", dpi=300)
