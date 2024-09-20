library(tidyverse)


# ------------------------------------------------------------------------------
# One chart per year:
#   - Display amount of money invested in each country.

pb |> 
  mutate(purchase_year = year(date_of_purchase)) |> 
  group_by(purchase_year, country) |> 
  arrange(country) |> 
  summarize(
    sum = sum(invested_amount)
  ) |> 
  ggplot(aes(x = country, y = sum)) +
  geom_col() +
  facet_wrap(~purchase_year) +
  guides(x = guide_axis(angle = 90)) +
  labs(
    title = "Investment amount by country (per year)",
    x = "Country",
    y = "Amount"
  )

# Save
ggsave("charts/investment-amount-by-country-in-each-year.png", width=30, height=20, units="cm", dpi=300)
