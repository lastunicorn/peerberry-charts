library(tidyverse)


# ------------------------------------------------------------------------------
# One chart per country:
#   - Display amount of money invested in each year.

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(year = year(date)) |> 
  mutate(year = as.factor(year)) |> 
  group_by(country, year) |> 
  arrange(country) |> 
  summarize(
    sum = sum(amount),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = year, y = sum)) +
  geom_col() +
  facet_wrap(~ country) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Investment amount by year (per country)",
    x = "Purchease year",
    y = "Amount"
  )

# Save
ggsave("charts/investment-amount-by-year-in-each-country.png", width=30, height=20, units="cm", dpi=300)
