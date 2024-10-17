library(tidyverse)


# ------------------------------------------------------------------------------
# Investment count in each country (per year)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(year = year(date)) |> 
  arrange(year) |> 
  group_by(country) |> 
  ggplot(aes(x = country)) +
  geom_bar() +
  facet_wrap(~ year, ncol = 1) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Investment count by country (per year)",
    x = "Country",
    y = "Count"
  )

save_plot("investment-count-by-country-per-year.png")
