library(tidyverse)


# ------------------------------------------------------------------------------
# One chart per year:
#   - Display amount of money invested in each country.

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(year = year(date)) |> 
  group_by(year, country) |> 
  summarize(
    sum = sum(amount),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = country, y = sum)) +
  geom_col() +
  facet_wrap(~ year, ncol = 1) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Investment amount by country (per year)",
    x = "Country",
    y = "Amount"
  )

# Save
ggsave("charts/investment-amount-by-country-in-each-year.png", width=30, height=20, units="cm", dpi=300)
