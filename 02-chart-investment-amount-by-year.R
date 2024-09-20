library(tidyverse)


# ------------------------------------------------------------------------------
# One chart per country:
#   - Display amount of money invested in each year.

pb |> 
  mutate(purchase_year = year(date_of_purchase)) |> 
  mutate(purchase_year = as.factor(purchase_year)) |> 
  group_by(country, purchase_year) |> 
  arrange(country) |> 
  summarize(
    sum = sum(invested_amount)
  ) |> 
  ggplot(aes(x = purchase_year, y = sum)) +
  geom_col() +
  facet_wrap(~country) +
  guides(x = guide_axis(angle = 90)) +
  labs(
    title = "Investment amount by year (per country)",
    x = "Purchease year",
    y = "Amount"
  )

# Save
ggsave("charts/investment-amount-by-year-in-each-country.png", width=30, height=20, units="cm", dpi=300)
